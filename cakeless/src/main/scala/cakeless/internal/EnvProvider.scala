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
      case RaiseResolution => ConflictResolution.Raise
      case WarnResolution  => ConflictResolution.Warn
    }

    unrefinedT.collectFirst {
      case tpe if !zenv.contains(tpe) =>
        fail(s"Attempt to exclude ($tpe) which is not a part of zio.ZEnv! Please, do it manually")
    }

    val info = getCakeInfo[R](constructor(N), refinementExclusions = unrefinedT, depsExclusions = depsExclusions.toSet)
    import info._
    val dependenciesList = collectDependencies(depsTypesList, depsExclusions, resolution)
    ifDebug {
      println(s"Excluded types: $excludedTypes\nexcluded deps: $excludedDeps")
      println(sep)
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
    val envCode                   = q"""new $mainType(...$passConstructorParams) with ..$refinementsWithExclusions { ..$assignmentsWithExclusions }"""

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

    val expr = q"""
       import _root_.zio._
       $createExcluder
     """

    ifDebug {
      println(expr)
    }
    expr
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

  private def collectDependencies(dependencies: List[Dependency], exclusions: List[Type], resolution: ConflictResolution): List[Tree] = {
    val members = enclosingClassBody
      .collect { case valDef: ValDef => valDef }
      .filterNot(_.mods.hasFlag(Flag.SYNTHETIC))
    //      .filterNot(_._1.rhs.isEmpty) todo: probably delete later
    //      .map(c.typecheck(_)) //
    //      .filterNot(_.tpe == null)

    ifDebug {
      println(s"""
           |Attempting to automatically wire dependencies...
           |defined values: $members
           |types: ${members.map { _.symbol.typeSignature }}
           |deps: $dependencies
      """.stripMargin)
    }

    val depsWithImpls: List[(Type, Tree)] = for {
      dependency @ Dependency(_, _, depTpe, _) <- dependencies
      if !exclusions.exists(depTpe.<:<)
    } yield {
      members.filter { _.symbol.typeSignature =:= depTpe } match {
        case List(valDef) =>
          depTpe -> selectDep(valDef, dependency, resolution)
        case Nil =>
          try depTpe -> c.typecheck(q"implicitly[$depTpe]")
          catch {
            case e: TypecheckException => fail(s"Not found value of type $depTpe in scope")
          }

        case multiple =>
          multiple filter {
            case ValDef(mods, _, _, _) => mods.annotations.exists(ann => c.typecheck(ann, c.TYPEmode).tpe =:= WiredAnnotation)
          } match {
            case List(valDef) => depTpe -> selectDep(valDef, dependency, resolution)
            case _            => fail(s"""
                 |Found multiple values of type $depTpe:
                 |${multiple.mkString("\n\tand ")}.
                 |You may choose one of them by just annotating it with @wired annotation
                 |""".stripMargin)
          }

      }
    }

    ifDebug {
      println(s"Wirings:\n\t${depsWithImpls.map { case (_, impl) => s"dep -> $impl" }.mkString("\n\t")}")
    }
    depsWithImpls.map(_._2)
  }

  private def selectDep(valDef: ValDef, dependency: Dependency, resolution: ConflictResolution): Tree = {
    import dependency.{prefixType, name => depName, tpe => depTpe, constructorInfo}
    import valDef.name

    def baseMessage: String = {
      val recommendedName =
        if (!(depName.toString.toLowerCase endsWith "impl")) s"${depName}Impl"
        else s"${depName}0"

      val fileLink = s"${valDef.pos.source.file}:${valDef.pos.line}:${valDef.pos.column}"
      s"""
           |Dependency `$depName` of type $depTpe (in type $prefixType) has the same name
           |as value `$name` (member of $enclosingCls, $fileLink).
           |For cake-pattern it may cause StackOverflowError due to cyclic reference.
           |It's better to rename `$depName` to (for instance) `$recommendedName`""".stripMargin
    }

    if (depName == name && constructorInfo.isEmpty) resolution match {
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
        q"$name"
    }
    else q"$name"
  }

  private val (enclosingCls, enclosingClassBody) = {
    val (cls, body) = c.enclosingClass match {
      case ClassDef(_, name, _, Template(parents, _, body)) => name.toTermName -> (parents ++ body)
      case ModuleDef(_, name, Template(parents, _, body))   => name            -> (parents ++ body)
      case e =>
        fail(s"Unknown type of enclosing class: ${e.getClass}")
    }

    cls -> (body ++ {
      val DefDef(_, _, _, _, _, body) = c.enclosingDef
      body
        .collect {
          case valDef: ValDef => valDef
        }
        .takeWhile(_.pos.line <= c.enclosingPosition.line)
    }).distinct
  }

  private val any             = typeOf[Any].dealias
  private val Zero            = typeOf[_0].dealias
  private val Inc             = typeOf[Nat.Inc[_0]].dealias
  private val RaiseResolution = typeOf[ConflictResolution.Raise].dealias
  private val WarnResolution  = typeOf[ConflictResolution.Warn].dealias
  private val WiredAnnotation = typeOf[wired].dealias

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
}
