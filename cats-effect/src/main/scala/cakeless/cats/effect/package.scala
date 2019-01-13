package cakeless.cats

import cakeless._
import _root_.cats.effect._
import _root_.cats.{~>, Id}
import scala.language.higherKinds

package object effect {
  implicit class SyncOps[A, D0](private val self: Cake.Aux[A, D0]) extends AnyVal {
    def delayed[F[_]](implicit F: Sync[F]): CakeT.Aux[F, A, D0] =
      self.mapK[F](λ[Id[?] ~> F[?]](F.delay(_)))
  }
}
