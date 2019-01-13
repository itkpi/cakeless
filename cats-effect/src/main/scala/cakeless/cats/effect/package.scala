package cakeless.cats

import cakeless._
import _root_.cats.effect._

package object effect {
  implicit def cakeIntoSync[F[_], A](implicit cake: Cake[A], F: Sync[F]): CakeT.Aux[F, A, cake.Dependencies] =
    new CakeT[F, A] {
      type Dependencies = cake.Dependencies
      def bake(deps: cake.Dependencies): F[A] = F.delay {
        cake bake deps
      }
    }
}
