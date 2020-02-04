package cakeless.internal

import scala.reflect.macros.{blackbox, TypecheckException}
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.Console
import zio.random.Random
import cakeless.nat._

class EnvProvider(override val c: blackbox.Context) extends DependencyResolver(c) {

  import c.universe._

  def mkConstructorImpl[R: WeakTypeTag, T >: R: WeakTypeTag, N <: Nat: WeakTypeTag]: Tree = {
    val R              = weakTypeOf[R].dealias
    val N              = weakTypeOf[N].dealias
    val T              = weakTypeOf[T].dealias
    val unrefinedT     = unrefine(T).toSet.filterNot(_ =:= any)
    val depsExclusions = zenvMembers.collect { case (k, _) if unrefinedT.contains(k) => k }.toList

    unrefinedT.collectFirst {
      case tpe if !zenv.contains(tpe) =>
        fail(s"Attempt to exclude ($tpe) which is not a part of zio.ZEnv! Please, do it manually")
    }

    val info = getCakeInfo[R](constructor(N), refinementExclusions = unrefinedT, depsExclusions = depsExclusions.toSet)
    import info._
    val dependenciesList = collectDependencies(depsTypesList, depsExclusions)
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
          new _root_.cakeless.internal.EnvConstructor[$R, $N] {
            type Excluded = $any
            override def construct(r: Excluded): UIO[$R] = UIO.effectTotal($envCode)
          }
         """
      case _ =>
        q"""
           new _root_.cakeless.internal.EnvConstructor[$R, $N] {
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

  private def collectDependencies(depsTypes: List[Type], exclusions: List[Type]): List[Tree] = {
    val clsBody = enclosingClassBody
    val members = clsBody
      .collect {
        case valDef: ValDef => valDef
      }
      .filterNot(_.rhs.isEmpty)
    //      .map(c.typecheck(_)) // todo: probably delete later
    //      .filterNot(_.tpe == null)

    ifDebug {
      println(s"""
           |Attempting to automatically wire dependencies...
           |defined values: $members
           |types: ${members.map(_.symbol.typeSignature)}
           |deps: $depsTypes
      """.stripMargin)
    }

    val depsWithImpls: List[(Type, Tree)] = for {
      dep <- depsTypes
      if !exclusions.exists(dep.<:<)
    } yield {
      members.filter(_.symbol.typeSignature =:= dep) match {
        case List(ValDef(_, _, _, impl)) => dep -> impl
        case Nil =>
          try dep -> c.typecheck(q"implicitly[$dep]")
          catch {
            case e: TypecheckException => fail(s"Not found value of type $dep in scope")
          }

        case multiple => fail(s"Found multiple values of type $dep:\n\t${multiple.mkString("\n\tand ")}\n\n")
      }
    }

    ifDebug {
      println(s"Wirings:\n\t${depsWithImpls.map { case (_, impl) => s"dep -> $impl" }.mkString("\n\t")}")
    }
    depsWithImpls.map(_._2)
  }

  private def enclosingClassBody: List[Tree] =
    ((c.enclosingClass match {
      case ClassDef(_, _, _, Template(parents, _, body)) => parents ++ body
      case ModuleDef(_, _, Template(parents, _, body))   => parents ++ body
      case e =>
        fail(s"Unknown type of enclosing class: ${e.getClass}")
    }) ++ {
      val DefDef(_, _, _, _, _, body) = c.enclosingDef
      body
        .collect {
          case valDef: ValDef => valDef
        }
        .takeWhile(_.pos.line <= c.enclosingPosition.line)
    }).distinct

  private val any  = typeOf[Any].dealias
  private val Zero = typeOf[_0].dealias
  private val Inc  = typeOf[Nat.Inc[_0]].dealias

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
