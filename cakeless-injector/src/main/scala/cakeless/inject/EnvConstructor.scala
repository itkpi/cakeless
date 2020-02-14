package cakeless.inject

import cakeless.ConflictResolution
import scala.language.experimental.macros
import cakeless.nat._

trait EnvConstructor[R, N <: Nat, CR <: ConflictResolution] {
  type Excluded

  def construct(r: Excluded): R
}

object EnvConstructor {
  type Aux[R, N <: Nat, CR <: ConflictResolution, Excluded0] = EnvConstructor[R, N, CR] { type Excluded = Excluded0 }
}
