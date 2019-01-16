package cakeless.lifecycle

import scala.language.higherKinds
import cakeless.CakeT.Aux
import cakeless._
import cats.{Applicative, MonadError}
import scala.annotation.implicitNotFound

/**
  * Typeclass allowing to control cake component lifecycle withing context [[F]]
  *
  * @tparam F - monadic context
  * */
@implicitNotFound("""Cannot manage lifecycle for cakes in context ${F}""")
trait Lifecycle[F[_]] {

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
  def preStart[A](cake: CakeT[F, A])(thunk: => Unit)(implicit F: Applicative[F]): CakeT.Aux[F, A, cake.Dependencies] =
    preStartF(cake)(F.pure(thunk))

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
  def postStart[A](cake: CakeT[F, A])(thunk: => Unit)(implicit F: Applicative[F]): CakeT.Aux[F, A, cake.Dependencies] =
    postStartF(cake)(F.pure(thunk))

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
  def postStartUse[A](cake: CakeT[F, A])(use: A => Unit)(implicit F: Applicative[F]): CakeT.Aux[F, A, cake.Dependencies] =
    postStartUseF(cake)(a => F.pure(use(a)))

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
  def handleErrorWith[A](cake: CakeT[F, A])(f: Throwable => F[A]): CakeT.Aux[F, A, cake.Dependencies]

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
  def handleError[A](cake: CakeT[F, A])(f: Throwable => A)(implicit F: Applicative[F]): CakeT.Aux[F, A, cake.Dependencies] =
    handleErrorWith(cake)(e => F.pure(f(e)))

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
  )(pf: PartialFunction[Throwable, F[A]]): CakeT.Aux[F, A, cake.Dependencies] =
    handleErrorWith(cake)(pf.applyOrElse(_, default = (e: Throwable) => throw e))

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
  def recover[A](cake: CakeT[F, A])(pf: PartialFunction[Throwable, A])(implicit F: Applicative[F]): CakeT.Aux[F, A, cake.Dependencies] =
    handleError(cake)(pf.applyOrElse(_, (e: Throwable) => throw e))
}

trait LowPriorityLifecycle {

  /**
    * Provides lifecycle capabilities for every context [[F]]
    * for which [[MonadError]] with fixed error type (Throwable) is defined
    * */
  implicit def fromMonadError[F[_]](implicit F: MonadError[F, Throwable]): Lifecycle[F] = new Lifecycle[F] {
    def preStartF[A](cake: CakeT[F, A])(thunk: => F[Unit]): Aux[F, A, cake.Dependencies] = new CakeT[F, A] {
      type Dependencies = cake.Dependencies

      def bake(deps: cake.Dependencies): F[A] = F.flatMap(thunk)(_ => cake bake deps)
    }

    def postStartF[A](cake: CakeT[F, A])(thunk: => F[Unit]): Aux[F, A, cake.Dependencies] = new CakeT[F, A] {
      type Dependencies = cake.Dependencies

      def bake(deps: cake.Dependencies): F[A] = F.flatTap(cake bake deps)(_ => thunk)
    }

    def postStartUseF[A](cake: CakeT[F, A])(use: A => F[Unit]): Aux[F, A, cake.Dependencies] = new CakeT[F, A] {
      type Dependencies = cake.Dependencies

      def bake(deps: cake.Dependencies): F[A] = F.flatTap(cake bake deps)(use)
    }

    def handleErrorWith[A](cake: CakeT[F, A])(
        f: Throwable => F[A]
    ): Aux[F, A, cake.Dependencies] = new CakeT[F, A] {
      type Dependencies = cake.Dependencies

      def bake(deps: cake.Dependencies): F[A] = F.handleErrorWith(cake bake deps)(f)
    }
  }
}

object Lifecycle extends LowPriorityLifecycle {
  def apply[F[_]](implicit ev: Lifecycle[F]): Lifecycle[F] = ev
}
