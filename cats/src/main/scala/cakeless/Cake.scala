package cakeless

import cats.Id
import shapeless.HNil

object Cake {

  /** alias for [[CakeT.Aux]] with [[Id]] context */
  type Aux[A, D0] = Cake[A] { type Dependencies = D0 }

  /**
    * @see [[CakeT.pure]]
    * */
  def pure[A](a: A): Cake.Aux[A, HNil] =
    new CakeT[Id, A] {
      type Dependencies = HNil

      def bake(deps: HNil): A = a
    }

  /**
    * @see [[CakeT.id]]
    * */
  def id[D0]: Cake.Aux[D0, D0] = CakeT.id[Id, D0]

  /**
    * @see [[CakeT.ap]]
    * */
  def ap[A, D0](f: D0 => A): Cake.Aux[A, D0] = CakeT.ap[Id, A, D0](f)
}
