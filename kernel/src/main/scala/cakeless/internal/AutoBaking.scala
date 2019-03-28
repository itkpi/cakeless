package cakeless.internal

import japgolly.microlibs.macro_utils.MacroUtils
import shapeless.HNil
import scala.language.experimental.macros
import scala.language.higherKinds
import scala.reflect.macros.{blackbox, TypecheckException}

trait AutoBake[F[_], A, D] {
  def apply(cakeT: CakeTBase[F, A] { type Dependencies = D }): F[A]
}

abstract class AutoBaking(val c: blackbox.Context) extends MacroUtils {
  import c.universe._

  def cakeImpl: Type

  private val hnil = typeOf[HNil].dealias

  def autoBakeImpl[F[_], A: WeakTypeTag, D: WeakTypeTag](implicit _F: WeakTypeTag[F[_]]): Tree = {
    val F = _F.tpe.dealias.typeConstructor
    val A = weakTypeOf[A].dealias
    val D = weakTypeOf[D].dealias

    val members = enclosingClassBody
      .collect {
        case valDef: ValDef if !valDef.toString().contains("cake[") && !valDef.toString().contains("auto") => valDef
      }
      .map(c.typecheck(_))
      .filterNot(_.tpe == null)

    val deps = hlistTypes(Nil, D)

    ifDebug {
      println(s"""
               |Attempting to automatically bake $cakeImpl[$F, $A, $D]...
               |defined values: $members
               |types: ${members.map(_.symbol.typeSignature)}
               |deps: $deps
      """.stripMargin)
    }

    val depsWithImpls: List[(Type, Tree)] = for (dep <- deps) yield {
      members.filter(_.symbol.typeSignature =:= dep) match {
        case List(ValDef(_, _, _, impl)) => dep -> impl
        case Nil =>
          try dep -> c.typecheck(q"implicitly[$dep]")
          catch { case e: TypecheckException => fail(s"Not found value of type $dep in scope") }

        case multiple => fail(s"Found multiple values of type $dep:\n\t${multiple.mkString("\n\tand ")}\n\n")
      }
    }

    ifDebug {
      println(s"Wirings:\n\t${depsWithImpls.map { case (dep, impl) => s"dep -> $impl" }.mkString("\n\t")}")
    }
    val depsHList = c.parse(depsWithImpls.map(_._2).mkString("", " :: ", " :: HNil"))
    val expr      = q"""
       new _root_.cakeless.internal.AutoBake[$F, $A, $D] {
         import shapeless._
         def apply(cakeT: _root_.cakeless.internal.CakeTBase[$F, $A] { type Dependencies = $D }) = cakeT.bake($depsHList)
       }
     """
    ifDebug {
      println(s"Generedated code:\n$expr\n$sep")
    }
    expr
  }

  private def enclosingClassBody: List[Tree] =
    ((c.enclosingClass match {
      case cd @ ClassDef(_, _, _, Template(parents, self, body)) => parents ++ body
      case md @ ModuleDef(_, _, Template(parents, self, body))   => parents ++ body
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

  private def hlistTypes(acc: List[Type], reminder: Type): List[Type] = reminder match {
    case `hnil` => acc
    case _ =>
      val List(head, tail) = reminder.typeArgs
      hlistTypes(acc :+ head, tail)
  }

  private val debug = sys.env.getOrElse("cakeless_macro_debug", "false").toBoolean
  private def ifDebug[U](thunk: => U): Unit =
    if (debug) thunk
}
