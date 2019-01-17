package cakeless.lifecycle

import cats.MonadError
import scala.language.{higherKinds, reflectiveCalls}

/**
  * Typeclass allowing to shutdown component [[A]]
  * in context [[F]]
  * */
trait Shutdown[F[_], A] {
  def shutdown(a: A): F[Unit]
}

trait LowPriorityShutdown {
  implicit def fromCloseable[F[_], A <: AutoCloseable](implicit F: MonadError[F, Throwable]): Shutdown[F, A] =
    new Shutdown[F, A] {
      def shutdown(a: A): F[Unit] = F.map(F.pure(a))(_.close())
    }

  implicit def forStructuralType[F[_], A <: { def shutdown(): Unit }](implicit F: MonadError[F, Throwable]): Shutdown[F, A] =
    new Shutdown[F, A] {
      def shutdown(a: A): F[Unit] = F.map(F.pure(a))(_.shutdown())
    }
}

object Shutdown extends LowPriorityShutdown {
  def apply[F[_], A](implicit ev: Shutdown[F, A]): Shutdown[F, A] = ev
}
