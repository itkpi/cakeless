package cakeless

import cats.Applicative

import scala.language.higherKinds
import scala.util.control.NonFatal

package object lifecycle {
  implicit class LifecycleOps[F[_], A, D0](private val self: CakeT.Aux[F, A, D0]) {
    def preStartF(thunk: => F[Unit])(implicit L: Lifecycle[F]): CakeT.Aux[F, A, D0] =
      L.preStartF(self)(thunk)

    def preStart(thunk: => Unit)(implicit F: Applicative[F], L: Lifecycle[F]): CakeT.Aux[F, A, D0] =
      L.preStart(self)(thunk)

    def postStartF(thunk: => F[Unit])(implicit L: Lifecycle[F]): CakeT.Aux[F, A, D0] =
      L.postStartF(self)(thunk)

    def postStart(thunk: => Unit)(implicit F: Applicative[F], L: Lifecycle[F]): CakeT.Aux[F, A, D0] =
      L.postStart(self)(thunk)

    def postStartUseF(use: A => F[Unit])(implicit L: Lifecycle[F]): CakeT.Aux[F, A, D0] =
      L.postStartUseF(self)(use)

    def postStartUse(use: A => Unit)(implicit F: Applicative[F], L: Lifecycle[F]): CakeT.Aux[F, A, D0] =
      L.postStartUse(self)(use)

    def handleErrorWith(f: Throwable => F[A])(implicit L: Lifecycle[F]): CakeT.Aux[F, A, D0] =
      L.handleErrorWith(self)(f)

    def handleError(f: Throwable => A)(implicit F: Applicative[F], L: Lifecycle[F]): CakeT.Aux[F, A, D0] =
      L.handleError(self)(f)

    def recoverWith(f: PartialFunction[Throwable, F[A]])(implicit L: Lifecycle[F]): CakeT.Aux[F, A, D0] =
      L.recoverWith(self)(f)

    def recover(f: PartialFunction[Throwable, A])(implicit F: Applicative[F], L: Lifecycle[F]): CakeT.Aux[F, A, D0] =
      L.recover(self)(f)

    def recoverNonFatal(thunk: => A)(implicit F: Applicative[F], L: Lifecycle[F]): CakeT.Aux[F, A, D0] =
      L.recover(self) { case NonFatal(_) => thunk }
  }
}
