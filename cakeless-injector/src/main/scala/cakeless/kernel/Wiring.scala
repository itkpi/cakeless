package cakeless.kernel

import cakeless.CollisionResolving
import cakeless.nat._

final class Wiring[A](private val `dummy`: Boolean = false) {
  def inject0: SimpleEnvInjector[A, _0, CollisionResolving.Default]         = new SimpleEnvInjector()
  def inject[N <: Nat]: SimpleEnvInjector[A, N, CollisionResolving.Default] = new SimpleEnvInjector()
}
