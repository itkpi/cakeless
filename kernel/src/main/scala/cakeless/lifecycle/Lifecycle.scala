package cakeless.lifecycle

import scala.language.higherKinds
import cakeless.CakeT.Aux
import cakeless._
import cats.{Applicative, MonadError}
import scala.annotation.implicitNotFound

@implicitNotFound("""Cannot manage lifecycle for cakes in context ${F}""")
trait Lifecycle[F[_]] {
  def preStartF[A](cake: CakeT[F, A])(thunk: => F[Unit]): CakeT.Aux[F, A, cake.Dependencies]

  def preStart[A](cake: CakeT[F, A])(thunk: => Unit)(implicit F: Applicative[F]): CakeT.Aux[F, A, cake.Dependencies] =
    preStartF(cake)(F.pure(thunk))

  def postStartF[A](cake: CakeT[F, A])(thunk: => F[Unit]): CakeT.Aux[F, A, cake.Dependencies]

  def postStart[A](cake: CakeT[F, A])(thunk: => Unit)(implicit F: Applicative[F]): CakeT.Aux[F, A, cake.Dependencies] =
    postStartF(cake)(F.pure(thunk))

  def postStartUseF[A](cake: CakeT[F, A])(use: A => F[Unit]): CakeT.Aux[F, A, cake.Dependencies]

  def postStartUse[A](cake: CakeT[F, A])(use: A => Unit)(implicit F: Applicative[F]): CakeT.Aux[F, A, cake.Dependencies] =
    postStartUseF(cake)(a => F.pure(use(a)))

  def handleErrorWith[A](cake: CakeT[F, A])(f: Throwable => F[A]): CakeT.Aux[F, A, cake.Dependencies]

  def handleError[A](cake: CakeT[F, A])(f: Throwable => A)(implicit F: Applicative[F]): CakeT.Aux[F, A, cake.Dependencies] =
    handleErrorWith(cake)(e => F.pure(f(e)))

  def recoverWith[A](
                      cake: CakeT[F, A]
                    )(f: PartialFunction[Throwable, F[A]]): CakeT.Aux[F, A, cake.Dependencies] =
    handleErrorWith(cake)(f.applyOrElse(_, default = (e: Throwable) => throw e))

  def recover[A](cake: CakeT[F, A])(f: PartialFunction[Throwable, A])(implicit F: Applicative[F]): CakeT.Aux[F, A, cake.Dependencies] =
    handleError(cake)(f.applyOrElse(_, (e: Throwable) => throw e))
}

trait LowPriorityLifecycle {
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
