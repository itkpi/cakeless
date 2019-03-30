package cakeless

import cakeless.internal.{CakeTBase, UnUnion, Union}
import cakeless.lifecycle.{Initialize, Shutdown}
import scalaz.zio._
import scalaz.zio.console._
import shapeless.{Generic, HNil, Nat}

import scala.annotation.unchecked.uncheckedVariance

trait CakeZ[-Effect, +Error, A] extends CakeTBase[ZIO[Effect, Error, ?] @uncheckedVariance, A] { self =>

  /**
    * Allows to convert [[self.Dependencies]] type into a case class
    *
    * @tparam R - typically case class whose generic representation is [[self.Dependencies]]
    * @param gen - `shapeless.Generic`
    * @return - same component with Dependencies as [[R]]
    * */
  def as[R](implicit gen: Generic.Aux[R, self.Dependencies]): CakeZ.Aux[Effect, Error, A, R] =
    comap[R](gen.to)

  def widen[D0 <: Dependencies]: CakeZ.Aux[Effect, Error, A, D0] =
    self.asInstanceOf[CakeZ.Aux[Effect, Error, A, D0]]

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
  def comap[D2](f: D2 => self.Dependencies): CakeZ.Aux[Effect, Error, A, D2] =
    new CakeZ[Effect, Error, A] {
      type Dependencies = D2

      def bake(deps: Dependencies): ZIO[Effect, Error, A] = self.bake(f(deps))
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
  def comapM[R1 <: Effect, E1 >: Error, D2](f: D2 => ZIO[R1, E1, self.Dependencies]): CakeZ.Aux[R1, E1, A, D2] =
    new CakeZ[R1, E1, A] {
      type Dependencies = D2

      def bake(deps: Dependencies): ZIO[R1, E1, A] = f(deps).flatMap(self.bake)
    }

  /**
    * Typical functor operation
    * allowing to transform wired component.
    *
    * @tparam B - new component type
    * @param f - transformation function
    * @return - cake for [[B]] component with same dependencies
    * */
  def map[B](f: A => B): CakeZ.Aux[Effect, Error, B, self.Dependencies] =
    new CakeZ[Effect, Error, B] {
      type Dependencies = self.Dependencies

      def bake(deps: Dependencies): ZIO[Effect, Error, B] = self bake deps map f
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
    * @return - cake for [[B]] component with new dependencies [[D2]]
    * */
  def bimap[D2, B](f: D2 => self.Dependencies, g: A => B): CakeZ.Aux[Effect, Error, B, D2] =
    comap(f).map(g)

  /**
    * Dependent map operation
    * allowing to transform wired component
    * using also [[self.Dependencies]] which will be provided later
    *
    * @tparam B - new component type
    * @param f - transformation function
    * @return - cake for [[B]] component with same dependencies
    * */
  def depMap[B](f: (self.Dependencies, A) => B): CakeZ.Aux[Effect, Error, B, self.Dependencies] =
    new CakeZ[Effect, Error, B] {
      type Dependencies = self.Dependencies

      def bake(deps: Dependencies): ZIO[Effect, Error, B] = (self bake deps).map(f(deps, _))
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
    * @return - cake for [[B]] component with new dependencies [[Out]]
    * */
  def flatMap[R1 <: Effect, E1 >: Error, B, D1, Out, AL <: Nat](
      f: A => CakeZ.Aux[R1, E1, B, D1]
  )(implicit union: Union.Aux[self.Dependencies, D1, Out], unUnion: UnUnion[self.Dependencies, D1, Out]): CakeZ.Aux[R1, E1, B, Out] =
    new CakeZ[R1, E1, B] {
      type Dependencies = Out

      def bake(deps: Dependencies): ZIO[R1, E1, B] = {
        val (deps0, deps1) = unUnion(deps)
        self bake deps0 flatMap { a =>
          f(a) bake deps1
        }
      }
    }

  /**
    * Allows to flatten component type
    * when [[A]] =:= [[B]]
    * */
  def flatten[R1 <: Effect, E1 >: Error, B](implicit ev: A <:< ZIO[R1, E1, B]): CakeZ.Aux[R1, E1, B, self.Dependencies] =
    new CakeZ[R1, E1, B] {
      type Dependencies = self.Dependencies

      def bake(deps: Dependencies): ZIO[R1, E1, B] = self.bake(deps).flatten
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
    * @return - cake for [[B]] component with new dependencies [[Out]]
    * */
  def depFlatMap[R1 <: Effect, E1 >: Error, B, D1, Out, AL <: Nat](
      f: (self.Dependencies, A) => CakeZ.Aux[R1, E1, B, D1]
  )(implicit union: Union.Aux[self.Dependencies, D1, Out], unUnion: UnUnion[self.Dependencies, D1, Out]): CakeZ.Aux[R1, E1, B, Out] =
    new CakeZ[R1, E1, B] {
      type Dependencies = Out

      def bake(deps: Dependencies): ZIO[R1, E1, B] = {
        val (deps0, deps1) = unUnion(deps)
        self bake deps0 flatMap { a =>
          f(deps0, a) bake deps1
        }
      }
    }

  /**
    * Monadic version of [[map]]
    * allowing to transform wired component
    * into value [[B]] within ZIO context
    *
    * @tparam B - new component type
    * @param f - contextual transformation function
    * @return - cake for [[B]] component with same dependencies
    * */
  def mapM[R1 <: Effect, E1 >: Error, B](f: A => ZIO[R1, E1, B]): CakeZ.Aux[R1, E1, B, self.Dependencies] =
    new CakeZ[R1, E1, B] {
      type Dependencies = self.Dependencies

      def bake(deps: Dependencies): ZIO[R1, E1, B] = self bake deps flatMap f
    }

  /**
    * @param msg - log record to be written after [[A]] instantiation
    * @return - same cake in context `WriterT[F, L, ?}`
    * */
  def logged(msg: String): CakeZ.Aux[Console with Effect, Error, A, self.Dependencies] =
    new CakeZ[Console with Effect, Error, A] {
      type Dependencies = self.Dependencies
      def bake(deps: Dependencies): ZIO[Console with Effect, Error, A] = self.bake(deps) <* putStrLn(msg)
    }

  def preStartF[R1 <: Effect, E1 >: Error](thunk: ZIO[R1, E1, _]): CakeZ.Aux[R1, E1, A, self.Dependencies] =
    new CakeZ[R1, E1, A] {
      type Dependencies = self.Dependencies

      def bake(deps: Dependencies): ZIO[R1, E1, A] = thunk zipRight (self bake deps)
    }

  def postStartF[R1 <: Effect, E1 >: Error](thunk: ZIO[R1, E1, _]): CakeZ.Aux[R1, E1, A, self.Dependencies] =
    new CakeZ[R1, E1, A] {
      type Dependencies = self.Dependencies

      def bake(deps: Dependencies): ZIO[R1, E1, A] = (self bake deps).zipLeft(thunk)
    }

  def postStartUseF[R1 <: Effect, E1 >: Error](use: A => ZIO[R1, E1, _]): CakeZ.Aux[R1, E1, A, self.Dependencies] =
    new CakeZ[R1, E1, A] {
      type Dependencies = self.Dependencies

      def bake(deps: Dependencies): ZIO[R1, E1, A] = (self bake deps).flatMap { a =>
        use(a) zipRight ZIO.succeed(a)
      }
    }

  def catchSome[R1 <: Effect, E1 >: Error](pf: PartialFunction[Error, A]): CakeZ.Aux[R1, E1, A, self.Dependencies] =
    new CakeZ[R1, E1, A] {
      type Dependencies = self.Dependencies

      def bake(deps: Dependencies): ZIO[R1, E1, A] = self bake deps catchSome {
        case e if pf isDefinedAt e => ZIO.succeedLazy(pf(e))
      }
    }

  def catchAllWith[R1 <: Effect, E1 >: Error](
      f: Error => ZIO[R1, E1, A]
  ): CakeZ.Aux[R1, E1, A, self.Dependencies] =
    new CakeZ[R1, E1, A] {
      type Dependencies = self.Dependencies

      def bake(deps: self.Dependencies): ZIO[R1, E1, A] = self bake deps catchAll f
    }

  /**
    * Allows to execute side-effect
    * exactly before [[A]] is instantiated
    *
    * @param thunk - side-effect
    * @return - same cake for which side-effect will be executed before [[A]] is instantiated
      **/
  def preStart(thunk: => Unit): CakeZ.Aux[Effect, Error, A, self.Dependencies] =
    preStartF(ZIO.succeedLazy(thunk))

  /**
    * Allows to execute side-effect
    * after [[A]] is successfully instantiated
    *
    * @param thunk - monadic side-effect
    * @return - same cake for which side-effect will be executed after [[A]] is successfully instantiated
      **/
  def postStart(thunk: => Unit): CakeZ.Aux[Effect, Error, A, self.Dependencies] =
    postStartF(ZIO.succeedLazy(thunk))

  /**
    * Allows to execute side-effect
    * depending on [[A]] component
    * after [[A]] is successfully instantiated.
    *
    * Useful for logging or other pure operations.
    *
    *\    * @param use  - component-dependent side-effect
    *
    * @return - same cake for which side-effect will be executed after [[A]] is successfully instantiated
      **/
  def postStartUse(use: A => Unit): CakeZ.Aux[Effect, Error, A, self.Dependencies] =
    postStartUseF(a => ZIO.succeedLazy(use(a)))

  /**
    * Allows to handle instantiation errors
    * providing new [[A]] component
    * dependent on specific error.
    *
    * @param f    - error-handling function
    * @return - same cake if no errors occurred or new component based on specific error
      **/
  def catchAll(f: Error => A): CakeZ.Aux[Effect, Error, A, self.Dependencies] =
    new CakeZ[Effect, Error, A] {
      type Dependencies = self.Dependencies

      def bake(deps: Dependencies): ZIO[Effect, Error, A] = self bake deps catchAll (e => ZIO.succeedLazy(f(e)))
    }

  /**
    * Allows to handle instantiation errors
    * providing new [[A]] component in context [[F]]
    * if provided partial function is defined on occurred error
    *
    * @param pf   - error-handling function
    * @return - same cake if no errors occurred or new component based on thrown error if PartialFunction is defined on that error.
      **/
  def catchSomeWith[R1 <: Effect, E1 >: Error](pf: PartialFunction[Error, ZIO[R1, E1, A]]): CakeZ.Aux[R1, E1, A, self.Dependencies] =
    new CakeZ[R1, E1, A] {
      type Dependencies = self.Dependencies

      def bake(deps: Dependencies): ZIO[R1, E1, A] = self bake deps catchSome pf
    }

  def fold[B](err: Error => B, succ: A => B): CakeZ.Aux[Effect, Nothing, B, self.Dependencies] =
    new CakeZ[Effect, Nothing, B] {
      type Dependencies = self.Dependencies

      def bake(deps: Dependencies): ZIO[Effect, Nothing, B] = (self bake deps).fold(err, succ)
    }

  def either: CakeZ.Aux[Effect, Nothing, Either[Error @uncheckedVariance, A], self.Dependencies] =
    new CakeZ[Effect, Nothing, Either[Error, A]] {
      type Dependencies = self.Dependencies

      def bake(deps: Dependencies): ZIO[Effect, Nothing, Either[Error, A]] = (self bake deps).either
    }

  def foldM[R1 <: Effect, E2, B](err: Error => ZIO[R1, E2, B], succ: A => ZIO[R1, E2, B]): CakeZ.Aux[R1, E2, B, self.Dependencies] =
    new CakeZ[R1, E2, B] {
      type Dependencies = self.Dependencies

      def bake(deps: Dependencies): ZIO[R1, E2, B] = (self bake deps).foldM(err, succ)
    }

  def mapError[E2](f: Error => E2): CakeZ.Aux[Effect, E2, A, self.Dependencies] =
    new CakeZ[Effect, E2, A] {
      type Dependencies = self.Dependencies

      def bake(deps: Dependencies): ZIO[Effect, E2, A] = self bake deps mapError f
    }

  def flatMapError[R1 <: Effect, E2](f: Error => ZIO[R1, Nothing, E2]): CakeZ.Aux[R1, E2, A, self.Dependencies] =
    new CakeZ[R1, E2, A] {
      type Dependencies = self.Dependencies

      def bake(deps: Dependencies): ZIO[R1, E2, A] = self bake deps flatMapError f
    }

  def bimapValue[E2, B](err: Error => E2, succ: A => B): CakeZ.Aux[Effect, E2, B, self.Dependencies] =
    new CakeZ[Effect, E2, B] {
      type Dependencies = self.Dependencies

      def bake(deps: Dependencies): ZIO[Effect, E2, B] = (self bake deps).bimap(err, succ)
    }

  def withInitialize[R1 <: Effect, E1 >: Error](
      implicit initialize: Initialize[ZIO[R1, E1, ?], A]
  ): CakeZ.Aux[R1, E1, A, self.Dependencies] =
    postStartUseF(initialize.initialize)

  def bracket[R1 <: Effect, E1 >: Error, B](
      release: A => ZIO[R1, Nothing, _]
  )(use: A => ZIO[R1, E1, B]): CakeZ.Aux[R1, E1, B, self.Dependencies] =
    new CakeZ[R1, E1, B] {
      type Dependencies = self.Dependencies

      def bake(deps: Dependencies): ZIO[R1, E1, B] = self.bake(deps).bracket(release)(use)
    }

  def withShutdown[R1 <: Effect, E1 >: Error, B](
      use: A => ZIO[R1, E1, B]
  )(implicit shutdown: Shutdown[ZIO[R1, Nothing, ?], A]): CakeZ.Aux[R1, E1, B, self.Dependencies] =
    bracket[R1, E1, B](shutdown.shutdown)(use)
}

object CakeZ {
  type Aux[-Effect, +E, A, D0] = CakeZ[Effect, E, A] { type Dependencies = D0 }

  /**
    * Allows to wrap pure value into [[CakeZ]]
    *
    * @tparam A - component type
    * @param a - pure component value
    * @return - cake with zero dependencies and already evaluated component
    * */
  def succeed[A](a: A): CakeZ.Aux[Any, Nothing, A, HNil] =
    new CakeZ[Any, Nothing, A] {
      type Dependencies = HNil

      def bake(deps: Dependencies): ZIO[Any, Nothing, A] = ZIO.succeed(a)
    }

  def succeedLazy[A](a: => A): CakeZ.Aux[Any, Nothing, A, HNil] =
    new CakeZ[Any, Nothing, A] {
      type Dependencies = HNil

      def bake(deps: Dependencies): ZIO[Any, Nothing, A] = ZIO.succeedLazy(a)
    }

  def fail[E](e: E): CakeZ.Aux[Any, E, Nothing, HNil] =
    new CakeZ[Any, E, Nothing] {
      type Dependencies = HNil

      def bake(deps: Dependencies): ZIO[Any, E, Nothing] = ZIO.fail(e)
    }

  /**
    * Allows to wrap component (in ZIO context) into [[CakeZ]]
    *
    * @tparam A - component type
    * @param fa - component value wrapped into [[F]] context
    * @return - cake with zero dependencies and already evaluated component
    * */
  def liftF[Effect, Error, A](fa: ZIO[Effect, Error, A]): CakeZ.Aux[Effect, Error, A, HNil] =
    new CakeZ[Effect, Error, A] {
      type Dependencies = HNil

      def bake(deps: Dependencies): ZIO[Effect, Error, A] = fa
    }

  /**
    * Allows to create cake with same component and dependencies type.
    * Its bake function will just return dependency passed in
    * (like [[scala.Predef.identity]] function does for pure values)
    *
    * @tparam D0 - dependency type
    * @return - cake with [[D0]] dependency for [[D0]] component
    * */
  def id[D0]: CakeZ.Aux[Any, Nothing, D0, D0] =
    new CakeZ[Any, Nothing, D0] {
      type Dependencies = D0

      def bake(deps: Dependencies): ZIO[Any, Nothing, D0] = ZIO.succeed(deps)
    }

  /**
    * Allows to lift pure function into Cake.
    *
    * Useful when [[A]] component is not a cake.
    *
    * @tparam A - component type
    * @tparam D0 - dependency type
    * @param f - allocation function
    * @return  - cake for which `bake` === `f`
    * */
  def ap[A, D0](f: D0 => A): CakeZ.Aux[Any, Nothing, A, D0] =
    new CakeZ[Any, Nothing, A] {
      type Dependencies = D0

      def bake(deps: Dependencies): ZIO[Any, Nothing, A] = ZIO.succeed(f(deps))
    }

  /**
    * Allows to lift monadic function into Cake.
    *
    * Useful when [[A]] component is not a cake.
    *
    * @tparam A - component type
    * @tparam D0 - dependency type
    * @param f - monadic allocation function
    * @return  - cake for which `bake`  `f`
    * */
  def apF[Effect, Error, A, D0](f: D0 => ZIO[Effect, Error, A]): CakeZ.Aux[Effect, Error, A, D0] =
    new CakeZ[Effect, Error, A] {
      type Dependencies = D0

      def bake(deps: Dependencies): ZIO[Effect, Error, A] = f(deps)
    }
}
