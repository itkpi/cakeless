package cakeless.lifecycle

import cats.MonadError
import scala.language.{higherKinds, reflectiveCalls}


trait LowPriorityInitialize {
  implicit def initializeForStructuralType[F[_], A <: { def init(): Unit }](implicit F: MonadError[F, Throwable]): Initialize[F, A] =
    new Initialize[F, A] {
      override def initialize(a: A): F[Unit] = F.map(F.pure(a))(_.init())
    }

  implicit def initializeForStructuralType2[F[_], A <: { def initialize(): Unit }](implicit F: MonadError[F, Throwable]): Initialize[F, A] =
    new Initialize[F, A] {
      override def initialize(a: A): F[Unit] = F.map(F.pure(a))(_.initialize())
    }
}