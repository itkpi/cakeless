package cakeless.internal

import cakeless.CakeT
import scala.language.experimental.macros
import scala.language.higherKinds
import scala.reflect.macros.whitebox

class CakeTAutoBacking(override val c: whitebox.Context) extends AutoBaking(c) {
  import c.universe._
  def cakeImpl: c.universe.Type = weakTypeOf[CakeT[cats.Id, Any]].dealias.typeConstructor
}
