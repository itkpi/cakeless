package cakeless.internal

import cakeless.CakeT
import scala.language.higherKinds
import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import japgolly.microlibs.macro_utils.MacroUtils

trait AutoBake[F[_], A, D] {
  def apply(cakeT: CakeT.Aux[F, A, D]): F[A]
}

object AutoBake {
  implicit def materialize[F[_], A, D]: AutoBake[F, A, D] = macro AutoBaking.autoBakeImpl[F, A, D]
}

class AutoBaking(val c: whitebox.Context) extends MacroUtils {
  import c.universe._

  def autoBakeImpl[F[_], A: WeakTypeTag, D: WeakTypeTag](implicit _F: WeakTypeTag[F[_]]): Tree = {
    val F = _F.tpe.dealias.typeConstructor
    val A = weakTypeOf[A].dealias
    val D = weakTypeOf[D].dealias

    println(s"""
        |Attempting to automatically bake CakeT[$F, $A, $D]...
        |prefix: ${c.prefix}
      """.stripMargin)

    fail("Not implemented yet")
  }
}
