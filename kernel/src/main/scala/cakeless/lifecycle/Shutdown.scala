package cakeless.lifecycle

import scala.language.higherKinds

/**
  * Typeclass allowing to shutdown component [[A]]
  * in context [[F]]
  * */
trait Shutdown[F[_], A] {
  def shutdown(a: A): F[Unit]
}

object Shutdown  {
  def apply[F[_], A](implicit ev: Shutdown[F, A]): Shutdown[F, A] = ev
}
