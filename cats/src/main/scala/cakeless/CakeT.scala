package cakeless

import cakeless.internal.{AutoBake, CakeTBase, UnUnion, Union}
import cakeless.lifecycle.{Bracket, Initialize, Lifecycle, Shutdown}
import cats.{~>, Applicative, FlatMap, Functor, Monad, Monoid}
import cats.data.{ReaderT, WriterT}
import cats.effect.ExitCase
import shapeless.{Generic, HNil, Nat}

import scala.language.higherKinds
import scala.util.control.NonFatal

trait CakeT[F[_], A] extends CakeTBase[F, A] { self =>

  /**
    * Allows to convert [[self.Dependencies]] type into a case class
    *
    * @tparam R - typically case class whose generic representation is [[self.Dependencies]]
    * @param gen - `shapeless.Generic`
    * @param F - functor for [[F]] context
    * @return - same component with Dependencies as [[R]]
    * */
  def as[R](implicit gen: Generic.Aux[R, self.Dependencies], F: Functor[F]): CakeT.Aux[F, A, R] =
    comap[R](gen.to)

  /**
    * Typical co-functor operation
    * allowing to change [[self.Dependencies]] type for this cake.
    *
    * For instance, you may want to make [[self.Dependencies]] less wide
    * flattening dependencies with some already known values.
    *
    * @tparam D2 - new dependency type
    * @param f - transformation function
    * @return - cake for same component with updated dependency type
    * */
  def comap[D2](f: D2 => self.Dependencies): CakeT.Aux[F, A, D2] =
    new CakeT[F, A] {
      type Dependencies = D2

      def bake(deps: Dependencies): F[A] = self.bake(f(deps))
    }

  /**
    * [[comap]] like operation
    * allowing to change [[self.Dependencies]] type for this cake
    * using monadic transformation function
    *
    * For instance, you may want to make [[self.Dependencies]] less wide
    * flattening dependencies with some already known values.
    *
    * @tparam D2 - new dependency type
    * @param f - monadic transformation function
    * @return - cake for same component with updated dependency type
    * */
  def comapM[D2](f: D2 => F[self.Dependencies])(implicit F: FlatMap[F]): CakeT.Aux[F, A, D2] =
    new CakeT[F, A] {
      type Dependencies = D2

      def bake(deps: Dependencies): F[A] = F.flatMap(f(deps))(self.bake)
    }

  /**
    * Typical functor operation
    * allowing to transform wired component.
    *
    * @tparam B - new component type
    * @param f - transformation function
    * @param F - functor for [[F]] context
    * @return - cake for [[B]] component with same dependencies
    * */
  def map[B](f: A => B)(implicit F: Functor[F]): CakeT.Aux[F, B, self.Dependencies] =
    new CakeT[F, B] {
      type Dependencies = self.Dependencies

      def bake(deps: Dependencies): F[B] = F.map(self bake deps)(f)
    }

  /**
    * Typical bifunctor operation
    *
    * allowing to do [[comap]] and [[map]] simultaneously
    *
    * @tparam D2 - new dependency type
    * @param f - `comap` transformation function
    * @tparam B - new component type
    * @param g - `map` transformation function
    * @param F - functor for [[F]] context
    * @return - cake for [[B]] component with new dependencies [[D2]]
    * */
  def bimap[D2, B](f: D2 => self.Dependencies, g: A => B)(implicit F: Functor[F]): CakeT.Aux[F, B, D2] =
    comap(f).map(g)

  /**
    * Dependent map operation
    * allowing to transform wired component
    * using also [[self.Dependencies]] which will be provided later
    *
    * @tparam B - new component type
    * @param f - transformation function
    * @param F - functor for [[F]] context
    * @return - cake for [[B]] component with same dependencies
    * */
  def depMap[B](f: (self.Dependencies, A) => B)(implicit F: Functor[F]): CakeT.Aux[F, B, self.Dependencies] =
    new CakeT[F, B] {
      type Dependencies = self.Dependencies

      def bake(deps: Dependencies): F[B] = F.map(self bake deps)(f(deps, _))
    }

  /**
    * Typical monadic transformation
    * allowing to chain several components into one cake
    * accumulating both [[self.Dependencies]] and [[D1]]
    * into new dependency [[Out]]
    * which is union type so that the following holds:
    *
    * [[self.Dependencies]] ∪ [[D1]] =:= [[Out]]
    *
    * @tparam B - new component type
    * @tparam D1 - dependency type for `that` Cake
    * @param f - monadic transformation function
    * @param union - dependent type evaluating [[Out]]
    * @param unUnion - value providing ability to do operation reverse to `union`
    *                (required for `flatMap`)
    * @param F - FlatMap typeclass for context [[F]]
    * @return - cake for [[B]] component with new dependencies [[Out]]
    * */
  def flatMap[B, D1, Out, AL <: Nat](
      f: A => CakeT.Aux[F, B, D1]
  )(implicit union: Union.Aux[self.Dependencies, D1, Out],
    unUnion: UnUnion[self.Dependencies, D1, Out],
    F: FlatMap[F]): CakeT.Aux[F, B, Out] =
    new CakeT[F, B] {
      type Dependencies = Out

      def bake(deps: Dependencies): F[B] = {
        val (deps0, deps1) = unUnion(deps)
        F.flatMap(self bake deps0) { a =>
          f(a) bake deps1
        }
      }
    }

  /**
    * Allows to flatten component type
    * when [[A]] =:= [[B]] in context [[F]]
    * */
  def flatten[B](implicit ev: A <:< F[B], F: FlatMap[F]): CakeT.Aux[F, B, self.Dependencies] =
    new CakeT[F, B] {
      type Dependencies = self.Dependencies

      def bake(deps: Dependencies): F[B] = F.flatMap(self.bake(deps))(ev)
    }

  /**
    * Dependent version of [[flatMap]]
    * allowing to chain several components into one cake
    * accumulating both [[self.Dependencies]] and [[D1]]
    * into new dependency [[Out]]
    * using also [[self.Dependencies]] which will be provided later.
    *
    * For union type [[Out]] the following holds:
    *
    * [[self.Dependencies]] ∪ [[D1]] =:= [[Out]]
    *
    * @tparam B - new component type
    * @tparam D1 - dependency type for `that` Cake
    * @param f - dependent monadic transformation function
    * @param union - dependent type evaluating [[Out]]
    * @param unUnion - value providing ability to do operation reverse to `union`
    *                (required for `flatMap`)
    * @param F - FlatMap typeclass for context [[F]]
    * @return - cake for [[B]] component with new dependencies [[Out]]
    * */
  def depFlatMap[B, D1, Out, AL <: Nat](
      f: (self.Dependencies, A) => CakeT.Aux[F, B, D1]
  )(implicit union: Union.Aux[self.Dependencies, D1, Out],
    unUnion: UnUnion[self.Dependencies, D1, Out],
    F: FlatMap[F]): CakeT.Aux[F, B, Out] =
    new CakeT[F, B] {
      type Dependencies = Out

      def bake(deps: Dependencies): F[B] = {
        val (deps0, deps1) = unUnion(deps)
        F.flatMap(self bake deps0) { a =>
          f(deps0, a) bake deps1
        }
      }
    }

  /**
    * Monadic version of [[map]]
    * allowing to transform wired component
    * into value [[B]] within context [[F]]
    *
    * @tparam B - new component type
    * @param f - contextual transformation function
    * @param F - monad for [[F]] context
    * @return - cake for [[B]] component with same dependencies
    * */
  def mapM[B](f: A => F[B])(implicit F: Monad[F]): CakeT.Aux[F, B, self.Dependencies] =
    new CakeT[F, B] {
      type Dependencies = self.Dependencies

      def bake(deps: Dependencies): F[B] = F.flatMap(self bake deps)(f)
    }

  /**
    * Function allowing to shift computations
    * into another monadic context [[G]]
    * using higher-kinded function
    *
    * @see [[cats.arrow.FunctionK]]
    * @tparam G - new monadic context for this cake
    * @param arrow - (higher-kinded) arrow function for transforming [[F]] context into [[G]]
    * @return - same cake in new context [[G]]
    * */
  def mapK[G[_]](arrow: F ~> G): CakeT.Aux[G, A, self.Dependencies] = new CakeT[G, A] {
    type Dependencies = self.Dependencies

    def bake(deps: Dependencies): G[A] = arrow(self bake deps)
  }

  /**
    * Function allowing to transform cake into classic Reader monad
    *
    * @see [[cats.data.ReaderT]]
    * @return - reader monad
    * */
  def toReader: ReaderT[F, self.Dependencies, A] = ReaderT[F, self.Dependencies, A](self.bake)

  /**
    * Wraps existing [[F]] context into [[WriterT]] monad
    * with log type [[F]].
    *
    * Used for purely-functional logging.
    * Typically [[L]] is data-structure (like [[List]]) keeping logs as values.
    *
    * @see [[cats.data.WriterT]]
    * @tparam L - log type for which monoid is defined
    * @param logRecord - log record to be written after [[A]] instantiation
    * @param F - applicative functor for context [[F]]
    * @return - same cake in context `WriterT[F, L, ?}`
    * */
  def logged[L: Monoid](logRecord: L)(implicit F: Applicative[F]): CakeT.Aux[WriterT[F, L, ?], A, self.Dependencies] =
    new CakeT[WriterT[F, L, ?], A] {
      type Dependencies = self.Dependencies
      def bake(deps: Dependencies): WriterT[F, L, A] = WriterT.liftF(self bake deps).tell(logRecord)
    }

  /**
    * Allows to safely cast [[Dependencies]] type into more specific type.
    * Useful for untagged cake dependencies
    * (typically [[D0]] are tagged dependencies)
    *
    * @tparam D0 - more specific dependencies type
    * @return - same cake with more specific dependencies type
    * */
  def widen[D0 <: Dependencies]: CakeT.Aux[F, A, D0] =
    self.asInstanceOf[CakeT.Aux[F, A, D0]]

  /**
    * Lifecycle function.
    *
    * @see [[Lifecycle.preStartF]]
    * */
  def preStartF(thunk: => F[Unit])(implicit L: Lifecycle[F, CakeT]): CakeT.Aux[F, A, self.Dependencies] =
    L.preStartF(self)(thunk)

  /**
    * Lifecycle function.
    *
    * @see [[Lifecycle.preStart]]
    * */
  def preStart(thunk: => Unit)(implicit L: Lifecycle[F, CakeT]): CakeT.Aux[F, A, self.Dependencies] =
    L.preStart(self)(thunk)

  /**
    * Lifecycle function.
    *
    * @see [[Lifecycle.postStartF]]
    * */
  def postStartF(thunk: => F[Unit])(implicit L: Lifecycle[F, CakeT]): CakeT.Aux[F, A, self.Dependencies] =
    L.postStartF(self)(thunk)

  /**
    * Lifecycle function.
    *
    * @see [[Lifecycle.postStart]]
    * */
  def postStart(thunk: => Unit)(implicit L: Lifecycle[F, CakeT]): CakeT.Aux[F, A, self.Dependencies] =
    L.postStart(self)(thunk)

  /**
    * Lifecycle function.
    *
    * @see [[Lifecycle.postStartUseF]]
    * */
  def postStartUseF(use: A => F[Unit])(implicit L: Lifecycle[F, CakeT]): CakeT.Aux[F, A, self.Dependencies] =
    L.postStartUseF(self)(use)

  /**
    * Lifecycle function.
    *
    * @see [[Lifecycle.postStartUse]]
    * */
  def postStartUse(use: A => Unit)(implicit L: Lifecycle[F, CakeT]): CakeT.Aux[F, A, self.Dependencies] =
    L.postStartUse(self)(use)

  /**
    * Lifecycle function allowing to handle instantiation errors
    *
    * @see [[Lifecycle.handleErrorWith]]
    * */
  def handleErrorWith(f: Throwable => F[A])(implicit L: Lifecycle[F, CakeT]): CakeT.Aux[F, A, self.Dependencies] =
    L.handleErrorWith(self)(f)

  /**
    * Lifecycle function allowing to handle instantiation errors
    *
    * @see [[Lifecycle.handleError]]
    * */
  def handleError(f: Throwable => A)(implicit L: Lifecycle[F, CakeT]): CakeT.Aux[F, A, self.Dependencies] =
    L.handleError(self)(f)

  /**
    * Lifecycle function allowing to handle instantiation errors
    *
    * @see [[Lifecycle.recoverWith]]
    * */
  def recoverWith(f: PartialFunction[Throwable, F[A]])(implicit L: Lifecycle[F, CakeT]): CakeT.Aux[F, A, self.Dependencies] =
    L.recoverWith(self)(f)

  /**
    * Lifecycle function allowing to handle instantiation errors
    *
    * @see [[Lifecycle.recover]]
    * */
  def recover(f: PartialFunction[Throwable, A])(implicit L: Lifecycle[F, CakeT]): CakeT.Aux[F, A, self.Dependencies] =
    L.recover(self)(f)

  /**
    * Lifecycle function allowing to handle instantiation errors
    * Allows to catch `NonFatal` exceptions replacing failed [[A]] component with a new one.
    *
    * @see [[scala.util.control.NonFatal]]
    * @param thunk - replacement for possibly failed [[A]] component
    * @param F - applicative functor for context [[F]]
    * @param L - lifecycle control for context [[F]]
    * @return - same cake with new component provided
    * */
  def recoverNonFatal(thunk: => A)(implicit L: Lifecycle[F, CakeT]): CakeT.Aux[F, A, self.Dependencies] =
    L.recover(self) { case NonFatal(_) => thunk }

  def withInitialize(implicit initialize: Initialize[F, A], L: Lifecycle[F, CakeT]): CakeT.Aux[F, A, self.Dependencies] =
    L.postStartUseF(self)(initialize.initialize)

  def bracketCase[E, B](
      use: A => F[B]
  )(release: (A, ExitCase[E]) => F[Unit])(implicit F: Bracket[F, E]): CakeT.Aux[F, B, self.Dependencies] =
    F.bracketCase(self)(use)(release)

  def bracket[E, B](use: A => F[B])(release: A => F[Unit])(implicit F: Bracket[F, E]): CakeT.Aux[F, B, self.Dependencies] =
    F.bracket(self)(use)(release)

  def uncancelable[E](implicit F: Applicative[F], B: Bracket[F, E]): CakeT.Aux[F, A, self.Dependencies] =
    B.uncancelable[A](self)

  def guarantee[E](finalizer: F[Unit])(implicit F: Applicative[F], B: Bracket[F, E]): CakeT.Aux[F, A, self.Dependencies] =
    B.guarantee(self)(finalizer)

  def guaranteeCase[E](finalizer: ExitCase[E] => F[Unit])(
      implicit F: Applicative[F],
      B: Bracket[F, E]
  ): CakeT.Aux[F, A, self.Dependencies] =
    B.guaranteeCase(self)(finalizer)

  def withShutdown[B](use: A => F[B])(implicit shutdown: Shutdown[F, A], B: Bracket[F, Throwable]): CakeT.Aux[F, B, self.Dependencies] =
    B.bracket[A, B](self)(use)(shutdown.shutdown)
}

object CakeT {

  /** Auxiliary type for fixing CakeT dependent type */
  type Aux[F[_], A, D0] = CakeT[F, A] { type Dependencies = D0 }

  /**
    * Allows to wrap pure value into [[CakeT]]
    *
    * @tparam F - monadic context
    * @tparam A - component type
    * @param a - pure component value
    * @param F - applicative functor for context [[F]]
    * @return - cake with zero dependencies and already evaluated component
    * */
  def pure[F[_], A](a: A)(implicit F: Applicative[F]): CakeT.Aux[F, A, HNil] =
    liftF(F.pure(a))

  /**
    * Allows to wrap component (in context [[F]]) into [[CakeT]]
    *
    * @tparam F - monadic context
    * @tparam A - component type
    * @param fa - component value wrapped into [[F]] context
    * @return - cake with zero dependencies and already evaluated component
    * */
  def liftF[F[_], A](fa: F[A]): CakeT.Aux[F, A, HNil] =
    new CakeT[F, A] {
      type Dependencies = HNil

      def bake(deps: HNil): F[A] = fa
    }

  /**
    * Allows to create cake with same component and dependencies type.
    * Its bake function will just return dependency passed in
    * (like [[scala.Predef.identity]] function does for pure values)
    *
    * @tparam F - monadic context
    * @tparam D0 - dependency type
    * @param F - applicative functor for context [[F]]
    * @return - cake with [[D0]] dependency for [[D0]] component
    * */
  def id[F[_], D0](implicit F: Applicative[F]): CakeT.Aux[F, D0, D0] = new CakeT[F, D0] {
    type Dependencies = D0

    def bake(deps: Dependencies): F[D0] = F.pure(deps)
  }

  /**
    * Allows to lift pure function into Cake.
    *
    * Useful when [[A]] component is not a cake.
    *
    * @tparam F - monadic context
    * @tparam A - component type
    * @tparam D0 - dependency type
    * @param f - allocation function
    * @param F - applicative functor for [[F]] context
    * @return  - cake for which `bake` === `f`
    * */
  def ap[F[_], A, D0](f: D0 => A)(implicit F: Applicative[F]): CakeT.Aux[F, A, D0] =
    new CakeT[F, A] {
      type Dependencies = D0

      def bake(deps: Dependencies): F[A] = F.pure(f(deps))
    }

  /**
    * Allows to lift monadic function into Cake.
    *
    * Useful when [[A]] component is not a cake.
    *
    * @tparam F - monadic context
    * @tparam A - component type
    * @tparam D0 - dependency type
    * @param f - monadic allocation function
    * @param F - monad for F
    * @return  - cake for which `bake`  `f`
    * */
  def apF[F[_], A, D0](f: D0 => F[A])(implicit F: Monad[F]): CakeT.Aux[F, A, D0] =
    ap[F, F[A], D0](f).flatten
}
