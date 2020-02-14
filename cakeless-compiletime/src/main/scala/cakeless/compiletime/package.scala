package cakeless

import cakeless.inject.EnvConstructor
import cakeless.nat.Nat

import scala.language.experimental.macros

package object compiletime {
  implicit def materialize[R, T >: R, N <: Nat, CR <: ConflictResolution]: EnvConstructor.Aux[R, N, CR, T] =
    macro CompileTimeInjector.mkConstructorImpl[R, T, N, CR]
}
