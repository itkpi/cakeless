package cakeless.lifecycle

import scala.language.higherKinds

/**
  * Typeclass allowing to initialize component [[A]]
  * in context [[F]]
  * */
trait Initialize[F[_], A] {
  def initialize(a: A): F[Unit]
}

object Initialize {
  def apply[F[_], A](implicit ev: Initialize[F, A]): Initialize[F, A] = ev
}
