package cakeless.lifecycle

import cakeless._
import cats.MonadError
import scala.annotation.implicitNotFound
import scala.language.higherKinds

/**
  * Typeclass allowing to control cake component lifecycle withing context [[F]]
  *
  * @tparam F - monadic context
  * */
@implicitNotFound("""Cannot manage lifecycle for cakes in context ${F}""")
trait Lifecycle[F[_], E] {

  /**
    * Allows to execute monadic side-effect
    * exactly before [[A]] is instantiated
    *
    * @tparam A - component type
    * @param cake - cake
    * @param thunk - monadic side-effect
    * @return - same cake for which side-effect will be executed before [[A]] is instantiated
    * */
  def preStartF[A](cake: CakeT[F, A])(thunk: => F[Unit]): CakeT.Aux[F, A, cake.Dependencies]

  /**
    * Allows to execute side-effect
    * exactly before [[A]] is instantiated
    *
    * @tparam A - component type
    * @param cake - cake
    * @param thunk - side-effect
    * @return - same cake for which side-effect will be executed before [[A]] is instantiated
    * */
  def preStart[A](cake: CakeT[F, A])(thunk: => Unit): CakeT.Aux[F, A, cake.Dependencies]

  /**
    * Allows to execute monadic side-effect
    * after [[A]] is successfully instantiated
    *
    * @tparam A - component type
    * @param cake - cake
    * @param thunk - monadic side-effect
    * @return - same cake for which side-effect will be executed after [[A]] is successfully instantiated
    * */
  def postStartF[A](cake: CakeT[F, A])(thunk: => F[Unit]): CakeT.Aux[F, A, cake.Dependencies]

  /**
    * Allows to execute side-effect
    * after [[A]] is successfully instantiated
    *
    * @tparam A - component type
    * @param cake - cake
    * @param thunk - monadic side-effect
    * @return - same cake for which side-effect will be executed after [[A]] is successfully instantiated
    * */
  def postStart[A](cake: CakeT[F, A])(thunk: => Unit): CakeT.Aux[F, A, cake.Dependencies]

  /**
    * Allows to execute monadic side-effect
    * depending on [[A]] component
    * after [[A]] is successfully instantiated.
    *
    * Useful for acquiring resources before program is started
    * (for instance opening connection with Database
    * or starting cache).
    *
    * @tparam A - component type
    * @param cake - cake
    * @param use - component-dependent monadic side-effect
    * @return - same cake for which side-effect will be executed after [[A]] is successfully instantiated
    * */
  def postStartUseF[A](cake: CakeT[F, A])(use: A => F[Unit]): CakeT.Aux[F, A, cake.Dependencies]

  /**
    * Allows to execute side-effect
    * depending on [[A]] component
    * after [[A]] is successfully instantiated.
    *
    * Useful for logging or other pure operations.
    *
    * @tparam A - component type
    * @param cake - cake
    * @param use - component-dependent side-effect
    * @return - same cake for which side-effect will be executed after [[A]] is successfully instantiated
    * */
  def postStartUse[A](cake: CakeT[F, A])(use: A => Unit): CakeT.Aux[F, A, cake.Dependencies]

  /**
    * Allows to handle instantiation errors
    * providing new [[A]] component in context [[F]]
    * dependent on specific error.
    *
    * @tparam A - component type
    * @param cake - cake
    * @param f - error-handling function
    * @return - same cake if no errors occurred or new component based on specific error
    * */
  def handleErrorWith[A](cake: CakeT[F, A])(f: E => F[A]): CakeT.Aux[F, A, cake.Dependencies]

  /**
    * Allows to handle instantiation errors
    * providing new [[A]] component
    * dependent on specific error.
    *
    * @tparam A - component type
    * @param cake - cake
    * @param f - error-handling function
    * @return - same cake if no errors occurred or new component based on specific error
    * */
  def handleError[A](cake: CakeT[F, A])(f: E => A): CakeT.Aux[F, A, cake.Dependencies]

  /**
    * Allows to handle instantiation errors
    * providing new [[A]] component in context [[F]]
    * if provided partial function is defined on occurred error
    *
    * @tparam A - component type
    * @param cake - cake
    * @param pf - error-handling function
    * @return - same cake if no errors occurred or new component based on thrown error if PartialFunction is defined on that error.
    * */
  def recoverWith[A](
      cake: CakeT[F, A]
  )(pf: PartialFunction[E, F[A]]): CakeT.Aux[F, A, cake.Dependencies]

  /**
    * Allows to handle instantiation errors
    * providing new [[A]] component
    * if provided partial function is defined on occurred error
    *
    * @tparam A - component type
    * @param cake - cake
    * @param pf - error-handling function
    * @return - same cake if no errors occurred or new component based on thrown error if PartialFunction is defined on that error.
    * */
  def recover[A](cake: CakeT[F, A])(pf: PartialFunction[E, A]): CakeT.Aux[F, A, cake.Dependencies]
}

trait CatsLifecycle {

  /**
    * Provides lifecycle capabilities for every context [[F]]
    * for which [[MonadError]] with fixed error type (Throwable) is defined
    * */
  implicit def fromMonadError[F[_], E](implicit F: MonadError[F, E]): Lifecycle[F, E] =
    new Lifecycle[F, E] {
      final type CakeImpl[Fx[_], Ax] = CakeT[Fx, Ax]

      def preStartF[A](cake: CakeT[F, A])(thunk: => F[Unit]): CakeT.Aux[F, A, cake.Dependencies] =
        new CakeT[F, A] {
          type Dependencies = cake.Dependencies

          def bake(deps: cake.Dependencies): F[A] = F.flatMap(thunk)(_ => cake bake deps)
        }

      def postStartF[A](cake: CakeT[F, A])(thunk: => F[Unit]): CakeT.Aux[F, A, cake.Dependencies] =
        new CakeT[F, A] {
          type Dependencies = cake.Dependencies

          def bake(deps: cake.Dependencies): F[A] = F.flatTap(cake bake deps)(_ => thunk)
        }

      def postStartUseF[A](cake: CakeT[F, A])(use: A => F[Unit]): CakeT.Aux[F, A, cake.Dependencies] =
        new CakeT[F, A] {
          type Dependencies = cake.Dependencies

          def bake(deps: cake.Dependencies): F[A] = F.flatTap(cake bake deps)(use)
        }

      def recover[A](cake: CakeT[F, A])(pf: PartialFunction[E, A]): CakeT.Aux[F, A, cake.Dependencies] =
        new CakeT[F, A] {
          type Dependencies = cake.Dependencies

          def bake(deps: cake.Dependencies): F[A] = F.recover(cake bake deps)(pf)
        }

      def handleErrorWith[A](cake: CakeT[F, A])(
          f: E => F[A]
      ): CakeT.Aux[F, A, cake.Dependencies] =
        new CakeT[F, A] {
          type Dependencies = cake.Dependencies

          def bake(deps: cake.Dependencies): F[A] = F.handleErrorWith(cake bake deps)(f)
        }

      /**
        * Allows to execute side-effect
        * exactly before [[A]] is instantiated
        *
        * @tparam A - component type
        * @param cake  - cake
        * @param thunk - side-effect
        * @return - same cake for which side-effect will be executed before [[A]] is instantiated
      **/
      def preStart[A](cake: CakeT[F, A])(thunk: => Unit): CakeT.Aux[F, A, cake.Dependencies] =
        preStartF(cake)(F.pure(thunk))

      /**
        * Allows to execute side-effect
        * after [[A]] is successfully instantiated
        *
        * @tparam A - component type
        * @param cake  - cake
        * @param thunk - monadic side-effect
        * @return - same cake for which side-effect will be executed after [[A]] is successfully instantiated
      **/
      def postStart[A](cake: CakeT[F, A])(thunk: => Unit): CakeT.Aux[F, A, cake.Dependencies] =
        postStartF(cake)(F.pure(thunk))

      /**
        * Allows to execute side-effect
        * depending on [[A]] component
        * after [[A]] is successfully instantiated.
        *
        * Useful for logging or other pure operations.
        *
        * @tparam A - component type
        * @param cake - cake
        * @param use  - component-dependent side-effect
        * @return - same cake for which side-effect will be executed after [[A]] is successfully instantiated
      **/
      def postStartUse[A](cake: CakeT[F, A])(use: A => Unit): CakeT.Aux[F, A, cake.Dependencies] =
        postStartUseF(cake)(a => F.pure(use(a)))

      /**
        * Allows to handle instantiation errors
        * providing new [[A]] component
        * dependent on specific error.
        *
        * @tparam A - component type
        * @param cake - cake
        * @param f    - error-handling function
        * @return - same cake if no errors occurred or new component based on specific error
      **/
      def handleError[A](cake: CakeT[F, A])(f: E => A): CakeT.Aux[F, A, cake.Dependencies] =
        new CakeT[F, A] {
          type Dependencies = cake.Dependencies

          def bake(deps: cake.Dependencies): F[A] = F.handleError(cake bake deps)(f)
        }

      /**
        * Allows to handle instantiation errors
        * providing new [[A]] component in context [[F]]
        * if provided partial function is defined on occurred error
        *
        * @tparam A - component type
        * @param cake - cake
        * @param pf   - error-handling function
        * @return - same cake if no errors occurred or new component based on thrown error if PartialFunction is defined on that error.
        **/
      def recoverWith[A](cake: CakeT[F, A])(pf: PartialFunction[E, F[A]]): CakeT.Aux[F, A, cake.Dependencies] =
        new CakeT[F, A] {
          type Dependencies = cake.Dependencies

          def bake(deps: cake.Dependencies): F[A] = F.recoverWith(cake bake deps)(pf)
        }
    }
}

object Lifecycle extends CatsLifecycle
