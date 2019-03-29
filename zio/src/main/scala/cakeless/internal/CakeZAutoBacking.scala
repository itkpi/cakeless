package cakeless.internal

import cakeless.CakeZ
import scala.reflect.macros.whitebox

class CakeZAutoBacking(override val c: whitebox.Context) extends AutoBaking(c) {
  import c.universe._
  def cakeImpl: c.universe.Type = weakTypeOf[CakeZ[Any, Nothing, Any]].dealias.typeConstructor
}
