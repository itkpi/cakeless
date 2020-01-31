package cakeless.internal

import scala.reflect.macros.{whitebox, TypecheckException}
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.Console
import zio.random.Random

class EnvProvider(override val c: whitebox.Context) extends DependencyResolver(c) {

  import c.universe._

  def injectPrimaryImpl[R: WeakTypeTag, E: WeakTypeTag, A: WeakTypeTag](instance: Expr[InjectionMagnet[R, E, A]])(exclude: Tree): Tree =
    injectImpl[R, E, A](instance)(reify(0))(exclude)

  def injectImpl[R: WeakTypeTag, E: WeakTypeTag, A: WeakTypeTag](instance: Expr[InjectionMagnet[R, E, A]])(constructor: Expr[Int])(exclude: Tree): Tree = {
    val info = getCakeInfo[R](constructor, refinementExclusions = zenv, depsExclusions = zenvDeps)
    import info._
    val dependenciesList = collectDependencies(depsTypesList)
    ifDebug {
      println(s"Excluded types: $excludedTypes\nexcluded deps: $excludedDeps")
      println(sep)
    }
//    val depsValueName   = TermName("deps")
    val instanceName          = TermName(c.freshName("instance"))
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
    val refinementsWithExclusions = typeRefinements ++ excludedTypes
    val envCode                   = q"""new $mainType(...$passConstructorParams) with ..$refinementsWithExclusions { ..$assignmentsWithExclusions }"""

    val createInstanceCode = q"ZIO.effectTotal($envCode)"
    val provideCode = excludedTypes match {
      case Nil => q"""$instanceName.provideSome[$any](_ => $createInstanceCode)"""
      case List(single) =>
        q"""
          $instanceName.provideSome[$single] { (x: $single) =>
             val $standardDepName = x
             $createInstanceCode
          }
         """

      case head :: rest =>
        q"""$instanceName.provideSome[$head with ..$rest] { (x: $head with ..$rest) =>
           val $standardDepName = x
           $createInstanceCode
        }"""
    }

    val expr = q"""
       import _root_.zio._
       val $instanceName  = $instance
       $provideCode
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
  ).map { case (k, v) => k.dealias -> v }

  private val zenvDeps: Set[Type] = {
    zenvMembers.map(_._1)
  }

  private def collectDependencies(depsTypes: List[Type]): List[Tree] = {
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
      if !zenvDeps.exists(dep.<:<)
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

  private val any = typeOf[Any].dealias
}
