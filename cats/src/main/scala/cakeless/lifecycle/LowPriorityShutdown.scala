package cakeless.lifecycle

import cats.MonadError
import scala.language.{higherKinds, reflectiveCalls}

trait LowPriorityShutdown {
  implicit def shutdownFromCloseable[F[_], A <: AutoCloseable](implicit F: MonadError[F, Throwable]): Shutdown[F, A] =
    new Shutdown[F, A] {
      def shutdown(a: A): F[Unit] = F.map(F.pure(a))(_.close())
    }

  implicit def shutdownForStructuralType[F[_], A <: { def shutdown(): Unit }](implicit F: MonadError[F, Throwable]): Shutdown[F, A] =
    new Shutdown[F, A] {
      def shutdown(a: A): F[Unit] = F.map(F.pure(a))(_.shutdown())
    }
}
