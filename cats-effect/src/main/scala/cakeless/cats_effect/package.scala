package cakeless

import cats.effect.{Bracket => _, _}
import cats.Applicative
import scala.language.experimental.macros
import scala.language.higherKinds

package object cats_effect {

  /**
    * Entry point for component wiring.
    * Allows to wrap [[A]] into [[CakeT]]
    * delaying [[A]] instantiation using context [[F]]
    * Requires [[cats.effect.Sync]] for context [[F]]
    *
    * [[A]] should be:
    * - abstract class with both constructor parameters and abstract `def`s or `val`s
    * [[A]] is allowed to have self-type requirements.
    * Dependencies of [[A]] self-types will be also picked-up
    *
    * */
  def cakeDelayed[F[_], A]: CakeT[F, A] = macro SyncResolver.makeCakeSync0[F, A]

  /**
    * Entry point for component wiring.
    * Allows to wrap [[A]] into [[CakeT]]
    * delaying [[A]] instantiation using context [[F]]
    * Requires [[cats.effect.Sync]] for context [[F]]
    *
    * `constructor` parameter allows to chose appropriate constructor.
    * If constructor is `0` - primary constructor will be used.
    * If constructor is `> 0` - constructor will be picked by definition order
    *
    * `constructor` is required to be literal value.
    * Otherwise code won't compile!!!
    *
    * [[A]] should be:
    * - abstract class with both constructor parameters and abstract `def`s or `val`s
    * [[A]] is allowed to have self-type requirements.
    * Dependencies of [[A]] self-types will be also picked-up
    *
    * @param constructor - sequence number of constructor whose type signature should be used as dependency type for cake
    * */
  def cakeDelayed[F[_], A](constructor: Int): CakeT[F, A] = macro SyncResolver.makeCakeSync[F, A]

  /**
    * Entry point for component wiring.
    * Allows to wrap [[A]] into [[CakeT]]
    * delaying [[A]] instantiation using context [[F]]
    * Requires [[cats.effect.Sync]] for context [[F]].
    *
    * Additionally ensures that [[A]] will be singleton component
    * (which means [[A]] will be allocated exactly once
    * no matter how much cake depends on it)
    * Implemented using [[cats.effect.concurrent.Ref]]
    *
    * [[A]] should be:
    * - abstract class with both constructor parameters and abstract `def`s or `val`s
    * [[A]] is allowed to have self-type requirements.
    * Dependencies of [[A]] self-types will be also picked-up
    *
    * */
  def cakeSingleton[F[_], A]: CakeT[F, A] = macro SyncResolver.makeCakeSyncSingleton0[F, A]

  /**
    * Entry point for component wiring.
    * Allows to wrap [[A]] into [[CakeT]]
    * delaying [[A]] instantiation using context [[F]]
    * Requires [[cats.effect.Sync]] for context [[F]].
    *
    * Additionally ensures that [[A]] will be singleton component
    * (which means [[A]] will be allocated exactly once
    * no matter how much cake depends on it)
    * Implemented using [[cats.effect.concurrent.Ref]]
    *
    * `constructor` parameter allows to chose appropriate constructor.
    * If constructor is `0` - primary constructor will be used.
    * If constructor is `> 0` - constructor will be picked by definition order
    *
    * `constructor` is required to be literal value.
    * Otherwise code won't compile!!!
    *
    * [[A]] should be:
    * - abstract class with both constructor parameters and abstract `def`s or `val`s
    * [[A]] is allowed to have self-type requirements.
    * Dependencies of [[A]] self-types will be also picked-up
    *
    * */
  def cakeSingleton[F[_], A](constructor: Int): CakeT[F, A] = macro SyncResolver.makeCakeSyncSingleton[F, A]

  /**
    * Bracket syntax for cakes.
    * */
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
