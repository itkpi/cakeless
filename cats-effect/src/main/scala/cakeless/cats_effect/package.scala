package cakeless

import cakeless._
import cats.effect.{Bracket => _, _}
import cats.{~>, Applicative, Id}
import scala.language.higherKinds

package object cats_effect {
  implicit class SyncOps[A, D0](private val self: Cake.Aux[A, D0]) extends AnyVal {
    def delayed[F[_]](implicit F: Sync[F]): CakeT.Aux[F, A, D0] =
      self.mapK[F](λ[Id[?] ~> F[?]](F.delay(_)))
  }

  implicit class BracketOps[F[_], A, D0](private val self: CakeT.Aux[F, A, D0]) extends AnyVal {

    def bracketCase[E, B](
        use: A => F[B]
    )(release: (A, ExitCase[E]) => F[Unit])(implicit F: Bracket[F, E]): CakeT.Aux[F, B, D0] =
      F.bracketCase(self)(use)(release)

    def bracket[E, B](use: A => F[B])(release: A => F[Unit])(implicit F: Bracket[F, E]): CakeT.Aux[F, B, D0] =
      F.bracket(self)(use)(release)

    def uncancelable[E](implicit F: Applicative[F], B: Bracket[F, E]): CakeT.Aux[F, A, D0] =
      B.uncancelable[A](self)

    def guarantee[E](finalizer: F[Unit])(implicit F: Applicative[F], B: Bracket[F, E]): CakeT.Aux[F, A, D0] =
      B.guarantee(self)(finalizer)

    def guaranteeCase[E](finalizer: ExitCase[E] => F[Unit])(implicit F: Applicative[F], B: Bracket[F, E]): CakeT.Aux[F, A, D0] =
      B.guaranteeCase(self)(finalizer)
  }
}
