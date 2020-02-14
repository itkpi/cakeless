package cakeless.internal

import cakeless.{wired, ConflictResolution}
import scala.reflect.macros.{blackbox, TypecheckException}
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.Console
import zio.random.Random
import cakeless.nat._

class EnvProvider(override val c: blackbox.Context) extends DependencyResolver(c) {

  import c.universe._

  def mkConstructorImpl[R: WeakTypeTag, T >: R: WeakTypeTag, N <: Nat: WeakTypeTag, CR <: ConflictResolution: WeakTypeTag]: Tree = {
    val R              = weakTypeOf[R].dealias
    val N              = weakTypeOf[N].dealias
    val T              = weakTypeOf[T].dealias
    val CR             = weakTypeOf[CR].dealias
    val unrefinedT     = unrefine(T).toSet.filterNot(_ =:= any)
    val depsExclusions = zenvMembers.collect { case (k, _) if unrefinedT.contains(k) => k }.toList
    val resolution = CR match {
      case AutoResolution  => ConflictResolution.Auto
      case RaiseResolution => ConflictResolution.Raise
      case WarnResolution  => ConflictResolution.Warn
    }

    unrefinedT.collectFirst {
      case tpe if !zenv.contains(tpe) =>
        fail(s"Attempt to exclude ($tpe) which is not a part of zio.ZEnv! Please, do it manually")
    }

    val info = getCakeInfo[R](constructor(N), refinementExclusions = unrefinedT, depsExclusions = depsExclusions.toSet)
    import info._
    val (dependenciesList, foundVals) = collectDependencies(depsTypesList, depsExclusions, resolution)
    debugged() {
      s"Excluded types: $excludedTypes\nexcluded deps: $excludedDeps\n$sep"
    }

    val resultingExclusions = excludedTypes.filter(unrefinedT.contains)

    val standardDepName       = TermName(c.freshName("zenv"))
    val passConstructorParams = passConstructorParamsIndexes.map(_.map(dependenciesList(_)))
    val assignmentsWithExclusions = {
      val overrides = assignmentWithIndexes.map {
        case (name, idx) => c.untypecheck(q"""override lazy val $name = ${dependenciesList(idx)}""")
      }
      val exclusions = excludedDeps.map { method =>
        val itsTpe = method.returnType.dealias
        val (_, selectionCode) = zenvMembers.find { case (zenvTpe, _) => itsTpe <:< zenvTpe }.getOrElse {
          fail(s"Not found ZEnv method for method $method with return type $itsTpe")
        }
        c.untypecheck(q"override lazy val ${method.name}: $itsTpe = $standardDepName.$selectionCode")
      }
      overrides ++ exclusions
    }
    val refinementsWithExclusions = typeRefinements ++ resultingExclusions
    val envCode =
      if (passConstructorParams.nonEmpty)
        q"""new $mainType(...$passConstructorParams) with ..$refinementsWithExclusions { ..$assignmentsWithExclusions }"""
      else q"""new $mainType with ..$refinementsWithExclusions { ..$assignmentsWithExclusions }"""

    val autoAssignments = resolution match {
      case ConflictResolution.Auto => foundVals.map(_.assignment)
      case _                       => Nil
    }
    val createExcluder = excludedTypes match {
      case Nil =>
        q"""
          new _root_.cakeless.internal.EnvConstructor[$R, $N, $CR] {
            type Excluded = $any
            override def construct(r: Excluded): UIO[$R] = UIO.effectTotal($envCode)
          }
         """
      case _ =>
        q"""
           new _root_.cakeless.internal.EnvConstructor[$R, $N, $CR] {
            type Excluded = $T
            override def construct($standardDepName: Excluded): UIO[$R] = UIO.effectTotal($envCode)
          }
         """
    }

    debugged(force = false) {
      val exprs = zioImport :: autoAssignments ::: List(createExcluder)
      q"""..$exprs"""
    }
  }

  private val zenv: Set[Type] = {
    Set(typeOf[Clock], typeOf[Console], typeOf[zio.system.System], typeOf[Random], typeOf[Blocking])
      .map(_.dealias)
  }

  private val zenvMembers = Set(
    typeOf[Clock.Service[_]]             -> TermName("clock"),
    typeOf[Console.Service[_]]           -> TermName("console"),
    typeOf[zio.system.System.Service[_]] -> TermName("system"),
    typeOf[Random.Service[_]]            -> TermName("random"),
    typeOf[Blocking.Service[_]]          -> TermName("blocking")
  ).map { case (k, v) => k.dealias -> v }.toMap

  private def collectDependencies(
      dependencies: List[Dependency],
      exclusions: List[Type],
      resolution: ConflictResolution
  ): (List[Tree], List[ValInfo]) = {
    debugged() {
      vals.mkString("\n")
    }

    debugged() {
      s"""
           |Attempting to automatically wire dependencies...
           |defined values: $vals
           |types: ${vals.map { _.valDef.symbol.typeSignature }}
           |deps: $dependencies
      """.stripMargin
    }

    val depsWithImpls: List[(Type, Tree, Option[ValInfo])] = for {
      dependency @ Dependency(_, _, depTpe, _) <- dependencies
      if !exclusions.exists(depTpe.<:<)
    } yield {
      vals.filter { _.tpe =:= depTpe } match {
        case List(valInfo) =>
          (depTpe, selectDep(valInfo, dependency, resolution), Some(valInfo))
        case Nil =>
          try {
            (depTpe, c.inferImplicitValue(depTpe), None)
          } catch {
            case e: TypecheckException => fail(s"Not found value of type $depTpe in scope")
          }

        case multiple =>
          multiple filter {
            case ValInfo(ValDef(mods, _, _, _), _) =>
              mods.annotations.exists(ann => c.typecheck(ann, c.TYPEmode).tpe =:= WiredAnnotation)
          } match {
            case List(valInfo) => (depTpe, selectDep(valInfo, dependency, resolution), Some(valInfo))
            case _             => fail(s"""
                 |Found multiple values of type $depTpe:
                 |${multiple.mkString("\n\tand ")}.
                 |You may choose one of them by just annotating it with @wired annotation
                 |""".stripMargin)
          }

      }
    }

    debugged() {
      s"Wirings:\n\t${depsWithImpls.map { case (_, impl, _) => s"dep -> $impl" }.mkString("\n\t")}"
    }
    depsWithImpls.map(_._2) -> depsWithImpls.flatMap(_._3)
  }

  private def selectDep(valInfo: ValInfo, dependency: Dependency, resolution: ConflictResolution): Tree = {
    import dependency.{prefixType, name => depName, tpe => depTpe, constructorInfo}
    import valInfo.valDef

    def baseMessage: String = {
      val recommendedName =
        if (!(depName.toString.toLowerCase endsWith "impl")) s"${depName}Impl"
        else s"${depName}0"

      s"""
           |Dependency `$depName` of type $depTpe (in type $prefixType) has the same name
           |as value $valInfo.
           |For cake-pattern it may cause StackOverflowError due to cyclic reference.
           |It's better to rename `$depName` to (for instance) `$recommendedName`""".stripMargin
    }

    if (depName == valDef.name && constructorInfo.isEmpty) resolution match {
      case ConflictResolution.Raise =>
        fail(
          s"""
               |$baseMessage
               |If you are sure what you're doing, use `.warnConflicts` on your EnvInjector
               |""".stripMargin
        )
      case ConflictResolution.Warn =>
        warn(
          s"""
               |$baseMessage
               |If you are not sure what you're doing, use `.raiseOnConflicts` on your EnvInjector  
               |""".stripMargin
        )
        q"${valDef.name}"

      case ConflictResolution.Auto =>
        q"${valInfo.freshName}"
    }
    else q"${valDef.name}"
  }

  private val vals: List[ValInfo] = {
    c.enclosingPackage match {
      case PackageDef(pkg, stats) =>
        debugged(force = false, extraInfo = s"in package ${pkg.name}") {
          ValInfoTraverser.use("package ${pkg.name}")(stats)
        }

      case e => fail(s"Unknown type of enclosing package: $e")
    }
  }

  private val any             = typeOf[Any].dealias
  private val Zero            = typeOf[_0].dealias
  private val Inc             = typeOf[Nat.Inc[_0]].dealias
  private val AutoResolution  = typeOf[ConflictResolution.Auto].dealias
  private val RaiseResolution = typeOf[ConflictResolution.Raise].dealias
  private val WarnResolution  = typeOf[ConflictResolution.Warn].dealias
  private val WiredAnnotation = typeOf[wired].dealias
  private val zioImport       = q"import _root_.zio._"

  private def constructor(N: Type): Int =
    if (N =:= Zero) 0
    else if (N <:< Inc) {
      N.typeArgs match {
        case List(n) => 1 + constructor(n)
        case _       => fail(s"Internal error: unknown Nat case of $N, expected either Zero or Inc")
      }
    } else {
      fail(s"Internal error: unknown Nat case of $N, expected either Zero or Inc")
    }

  case class ValInfo(valDef: ValDef, history: List[String]) {
    private def nonEmptyTree(tree: Tree): Option[Tree] = Option(tree) flatMap {
      case EmptyTree => None
      case t         => Some(t)
    }

    private def silentTpe(tree: Tree): Option[Type] =
      nonEmptyTree(
        c.typecheck(tree, c.TYPEmode, silent = true)
      ).flatMap(x => Option(x.tpe))

    private def concreteTpeOpt(tree: Tree): Option[Type] =
      Option(tree).flatMap(silentTpe).flatMap {
        case `any` => None
        case t     => Some(t)
      }

    def name: String = valDef.name.toString
    def tpe: Type =
      concreteTpeOpt(valDef.tpt) orElse
        concreteTpeOpt(valDef.rhs) orElse
        concreteTpeOpt(valDef) orElse
        Option(valDef.symbol.typeSignature) getOrElse {
        fail(s"""
             |Unable to determine type of ${toStringImpl()}.
             |Please provide type annotation to hint cakeless macro.
             |""".stripMargin)
      }

    val freshName: TermName = TermName(c.freshName(name))
    def assignment: Tree    = q""" val $freshName = ${valDef.name} """

    private def toStringImpl(tpeStr: Option[String] = None): String = {
      val theTpe   = tpeStr.getOrElse("<not inferred>")
      val fileLink = s"${valDef.pos.source.file}:${valDef.pos.line}:${valDef.pos.column}"
      s"""
         |{{{
         |  val $name: $theTpe = ${valDef.rhs}
         |  in ${history.mkString(" --> ")}
         |  $fileLink
         |}}}""".stripMargin
    }

    override def toString: String = toStringImpl(tpeStr = Some(tpe.toString))
  }

  private def isAccessibleFrom(tree: Tree)(from: Tree): Boolean = {
    c.echo(c.enclosingPosition, s"Checking accessibility of $tree from $from")
    true
  }

  object ValInfoTraverser extends Traverser {
    private var _infos: List[ValInfo]  = Nil
    private var _history: List[String] = Nil

    private def dive[U](modHistory: String*)(thunk: => U): U = {
      _history :::= modHistory.toList
      val res = thunk
      if (modHistory.nonEmpty) {
        _history = _history.dropRight(modHistory.size)
      }
      res
    }

    def use(history: String*)(trees: List[Tree]): List[ValInfo] = {
      dive(history: _*) {
        traverseTrees(trees)
      }
      val infos = _infos
      _infos = Nil
      _history = Nil
      infos
    }

    override def traverse(tree: c.universe.Tree): Unit =
      if (isAccessibleFrom(tree)(from = EmptyTree)) tree match {
        case valDef: ValDef if isAllowedToScrap(valDef) =>
          _infos ::= ValInfo(valDef, _history)

        case ModuleDef(_, name, Template(parents, _, body)) =>
          dive(s"object $name") {
            traverseTrees(parents)
            traverseTrees(body)
          }

        case ClassDef(_, name, _, Template(parents, _, body)) =>
          dive(s"class $name") {
            traverseTrees(parents)
            traverseTrees(body)
          }

        case DefDef(_, name, _, _, _, body) =>
          dive(s"def $name") {
            traverseTrees(collectValDefs(body))
          }

        case Apply(fun, body) =>
          dive(s"apply $fun") {
            traverseTrees(body)
          }

        case Block(stats, _) =>
          dive() {
            traverseTrees(stats)
          }

        case Function(vparams, body) =>
          dive("anonfun") {
            _infos :::= vparams.map(ValInfo(_, _history))
            _infos :::= collectValDefs(body).map(ValInfo(_, _history))
          }

        case other =>
      }
  }

  private def isAllowedToScrap(tree: Tree): Boolean =
    tree.pos.line <= c.enclosingPosition.line

  private def collectValDefs(body: Tree): List[ValDef] =
    body.collect {
      case valDef: ValDef if isAllowedToScrap(valDef) =>
        List(valDef)

      case Import(s, sels) =>
        val checkedImport = c.typecheck(s, mode = c.TYPEmode, silent = true)
        val isWildCard    = sels.exists(_.isWildcard)
        if (!isWildCard) Nil
        else {
          debugged(force = false) {
            val importMembers = (checkedImport.tpe.members.view.collect {
              case s if s.isPublic && s.isMethod && !s.isSynthetic =>
                s.asMethod
            } filter { s =>
              s.isVal || s.isCaseAccessor || {
                s.paramLists.flatten.isEmpty
              }
            } filterNot { s =>
              s.isJava || s.isImplementationArtifact
            } filterNot { s =>
              val str               = s.toString
              val needsToBeExcluded = hardcodedExcludedImports.exists(str.contains)
              debugged() {
                s"$str need bo be excluded? $needsToBeExcluded"
              }
              needsToBeExcluded
            }).toList
            importMembers.map(s => internal.valDef(s, EmptyTree))
          }
        }
    }.flatten

  private lazy val hardcodedExcludedImports = Set("##", "asInstanceOf", "isInstanceOf")

  implicit class triedEnclosure[A](self: => A) {
    def tried: String =
      try self.toString
      catch {
        case _: Throwable => "<empty>"
      }
  }

  implicit class loggedOps[A](self: A) {
    def loggedAs(desc: String): A = {
      println(s"[$desc] $self")
      self
    }
  }
}
