package cakeless.kernel

import cakeless.CollisionResolving
import scala.language.experimental.macros
import cakeless.nat._

trait EnvConstructor[R, N <: Nat, CR <: CollisionResolving] {
  type Excluded

  def construct(r: Excluded): R
}

object EnvConstructor {
  type Aux[R, N <: Nat, CR <: CollisionResolving, Excluded0] = EnvConstructor[R, N, CR] { type Excluded = Excluded0 }
}
