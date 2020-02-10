package cakeless.internal

import cakeless.ConflictResolution

import scala.language.experimental.macros
import cakeless.nat._
import zio.UIO

trait EnvConstructor[R, N <: Nat, CR <: ConflictResolution] {
  type Excluded

  def construct(r: Excluded): UIO[R]
}

object EnvConstructor {
  type Aux[R, N <: Nat, CR <: ConflictResolution, Excluded0] = EnvConstructor[R, N, CR] { type Excluded = Excluded0 }

  implicit def materialize[R, T >: R, N <: Nat, CR <: ConflictResolution]: EnvConstructor.Aux[R, N, CR, T] =
    macro EnvProvider.mkConstructorImpl[R, T, N, CR]
}
