package cakeless.lifecycle

import cakeless._
import cats.MonadError
import scala.language.higherKinds

trait CatsLifecycle {

  /**
    * Provides lifecycle capabilities for every context [[F]]
    * for which [[MonadError]] with fixed error type (Throwable) is defined
    * */
  implicit def fromMonadError[F[_]](implicit F: MonadError[F, Throwable]): Lifecycle[F, CakeT] = new Lifecycle[F, CakeT] {
    override type CakeImplAux[Fx[_], Ax, Deps] = CakeT.Aux[Fx, Ax, Deps]

    def preStartF[A](cake: CakeT[F, A])(thunk: => F[Unit]): CakeImplAux[F, A, cake.Dependencies] =
      new CakeT[F, A] {
        type Dependencies = cake.Dependencies

        def bake(deps: cake.Dependencies): F[A] = F.flatMap(thunk)(_ => cake bake deps)
      }

    def postStartF[A](cake: CakeT[F, A])(thunk: => F[Unit]): CakeImplAux[F, A, cake.Dependencies] =
      new CakeT[F, A] {
        type Dependencies = cake.Dependencies

        def bake(deps: cake.Dependencies): F[A] = F.flatTap(cake bake deps)(_ => thunk)
      }

    def postStartUseF[A](cake: CakeT[F, A])(use: A => F[Unit]): CakeImplAux[F, A, cake.Dependencies] =
      new CakeT[F, A] {
        type Dependencies = cake.Dependencies

        def bake(deps: cake.Dependencies): F[A] = F.flatTap(cake bake deps)(use)
      }

    def recover[A](cake: CakeT[F, A])(pf: PartialFunction[Throwable, A]): CakeImplAux[F, A, cake.Dependencies] =
      handleError(cake)(pf.applyOrElse(_, (e: Throwable) => throw e))

    def handleErrorWith[A](cake: CakeT[F, A])(
        f: Throwable => F[A]
    ): CakeImplAux[F, A, cake.Dependencies] =
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
    def preStart[A](cake: CakeT[F, A])(thunk: => Unit): CakeImplAux[F, A, cake.Dependencies] =
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
    def postStart[A](cake: CakeT[F, A])(thunk: => Unit): CakeImplAux[F, A, cake.Dependencies] =
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
    def postStartUse[A](cake: CakeT[F, A])(use: A => Unit): CakeImplAux[F, A, cake.Dependencies] =
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
    def handleError[A](cake: CakeT[F, A])(f: Throwable => A): CakeImplAux[F, A, cake.Dependencies] =
      handleErrorWith(cake)(e => F.pure(f(e)))
  }
}
