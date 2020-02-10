package cakeless.nat

/** simple phantom type for choosing appropriate constructor for DI */
sealed trait Nat
object Nat {
  sealed trait Zero          extends Nat
  sealed trait Inc[N <: Nat] extends Nat
}
