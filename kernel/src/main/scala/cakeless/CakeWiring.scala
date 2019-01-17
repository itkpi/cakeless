package cakeless

import scala.language.higherKinds

trait CakeWiring[F[_]] {
  type Component
  type Dependencies

  @inline def wire: CakeT.Aux[F, Component, Dependencies]
}
