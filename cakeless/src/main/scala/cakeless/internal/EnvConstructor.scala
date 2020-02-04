package cakeless.internal

import scala.language.experimental.macros
import cakeless.nat._
import zio.UIO

trait EnvConstructor[R, N <: Nat] {
  type Excluded

  def construct(r: Excluded): UIO[R]
}

object EnvConstructor {
  type Aux[R, N <: Nat, Excluded0] = EnvConstructor[R, N] { type Excluded = Excluded0 }

  implicit def materialize[R, T >: R, N <: Nat]: EnvConstructor.Aux[R, N, T] = macro EnvProvider.mkConstructorImpl[R, T, N]
}
