package cakeless.kernel

import cakeless.CollisionResolving
import cakeless.nat.Nat

class SimpleEnvInjector[A, N <: Nat, CR <: CollisionResolving](private val `dummy`: Boolean = false) extends AnyVal {
  def resolveCollisions[CR0 <: CollisionResolving]: SimpleEnvInjector[A, N, CR0] = new SimpleEnvInjector()

  @`inline` def wire(implicit constructor: EnvConstructor.Aux[A, N, CR, Any]): A =
    constructor.construct(())
}
