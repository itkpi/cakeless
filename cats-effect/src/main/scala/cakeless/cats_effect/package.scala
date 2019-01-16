package cakeless

import cats.effect.concurrent.Ref
import cats.effect.{Bracket => _, _}
import cats.Applicative
import scala.language.experimental.macros
import scala.language.higherKinds

package object cats_effect {
  def cakeDelayed[F[_], A]: CakeT[F, A] = macro SyncResolver.makeCakeSync0[F, A]
  def cakeDelayed[F[_], A](constructor: Int): CakeT[F, A] = macro SyncResolver.makeCakeSync[F, A]
  def cakeSingleton[F[_], A]: CakeT[F, A] = macro SyncResolver.makeCakeSyncSingleton0[F, A]
  def cakeSingleton[F[_], A](constructor: Int): CakeT[F, A] = macro SyncResolver.makeCakeSyncSingleton[F, A]

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
