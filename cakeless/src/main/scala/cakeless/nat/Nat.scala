package cakeless.nat

sealed trait Nat
object Nat {
  sealed trait Zero          extends Nat
  sealed trait Inc[N <: Nat] extends Nat
}
