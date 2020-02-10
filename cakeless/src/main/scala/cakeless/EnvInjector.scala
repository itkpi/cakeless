package cakeless

import cakeless.internal.EnvConstructor
import cakeless.nat.Nat
import zio._
import scala.language.implicitConversions

class EnvInjector0[R, E, A, N <: Nat, CR <: ConflictResolution](private val zio: ZIO[R, E, A]) {
  def withLifecycle[R1, R2](lifecycle: Lifecycle[R1, R2, R]): EnvInjector1[R, Any, E, A, R1, R2, N, CR] =
    new EnvInjector1[R, Any, E, A, R1, R2, N, CR](zio, lifecycle)

  def excludeZEnv[T]: EnvInjector1[_ >: R, T, E, A, Any, Any, N, CR] = new EnvInjector1(zio, Lifecycle.empty)

  def wire(implicit constructor: EnvConstructor.Aux[R, N, CR, Any]): ZIO[Any, E, A] =
    zio.provideSomeM[Any, E](
      ZIO.environment[Any].flatMap(constructor.construct)
    )

  def raiseOnConflicts: EnvInjector0[R, E, A, N, ConflictResolution.Raise] =
    this.asInstanceOf[EnvInjector0[R, E, A, N, ConflictResolution.Raise]]

  def warnConflicts: EnvInjector0[R, E, A, N, ConflictResolution.Warn] =
    this.asInstanceOf[EnvInjector0[R, E, A, N, ConflictResolution.Warn]]
}

class EnvInjector1[R, T, E, A, R1, R2, N <: Nat, CR <: ConflictResolution](
    private val zio: ZIO[R, E, A],
    private val lifecycle: Lifecycle[R1, R2, R]
) {

  def excludeZEnv[T0]: EnvInjector1[R, T0, E, A, R1, R2, N, CR] = new EnvInjector1(zio, lifecycle)

  def raiseOnConflicts: EnvInjector1[R, T, E, A, R1, R2, N, ConflictResolution.Raise] =
    this.asInstanceOf[EnvInjector1[R, T, E, A, R1, R2, N, ConflictResolution.Raise]]

  def warnConflicts: EnvInjector1[R, T, E, A, R1, R2, N, ConflictResolution.Warn] =
    this.asInstanceOf[EnvInjector1[R, T, E, A, R1, R2, N, ConflictResolution.Warn]]

  def wire(
      implicit constructor: EnvConstructor.Aux[R2 with R with R1, N, CR, T]
  ): ZIO[T, E, A] = {
    val construct = for {
      _   <- lifecycle.preStartURIO
      all <- ZIO.accessM[T](constructor.construct)
      _   <- lifecycle.postStartURIO(all)
      res <- zio.provide(all)
    } yield res

    construct.asInstanceOf[ZIO[T, E, A]]
  }
}
