package cakeless.lifecycle

import cats.MonadError
import scala.language.{higherKinds, reflectiveCalls}

/**
  * Typeclass allowing to initialize component [[A]]
  * in context [[F]]
  * */
trait Initialize[F[_], A] {
  def initialize(a: A): F[Unit]
}

trait LowPriorityInitialize {
  implicit def forStructuralType[F[_], A <: { def init(): Unit }](implicit F: MonadError[F, Throwable]): Initialize[F, A] =
    new Initialize[F, A] {
      override def initialize(a: A): F[Unit] = F.map(F.pure(a))(_.init())
    }

  implicit def forStructuralType2[F[_], A <: { def initialize(): Unit }](implicit F: MonadError[F, Throwable]): Initialize[F, A] =
    new Initialize[F, A] {
      override def initialize(a: A): F[Unit] = F.map(F.pure(a))(_.initialize())
    }
}

object Initialize extends LowPriorityInitialize {
  def apply[F[_], A](implicit ev: Initialize[F, A]): Initialize[F, A] = ev
}
