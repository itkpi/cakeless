package cakeless

import cakeless.internal.EnvConstructor
import cakeless.nat.Nat
import zio._
import scala.language.implicitConversions

class EnvInjector0[R, E, A, N <: Nat](private val zio: ZIO[R, E, A]) {
  def withLifecycle[R1, R2](lifecycle: Lifecycle[R1, R2, R]): EnvInjector1[R, Any, E, A, R1, R2, N] =
    new EnvInjector1[R, Any, E, A, R1, R2, N](zio, lifecycle)

  def excludeZEnv[T]: EnvInjector1[_ >: R, T, E, A, Any, Any, N] = new EnvInjector1(zio, Lifecycle.empty)

  def wire(implicit constructor: EnvConstructor.Aux[R, N, Any]): ZIO[Any, E, A] =
    zio.provideSomeM[Any, E](
      ZIO.environment[Any].flatMap(constructor.construct)
    )
}

class EnvInjector1[R, T, E, A, R1, R2, N <: Nat](private val zio: ZIO[R, E, A], private val lifecycle: Lifecycle[R1, R2, R]) {

  def excludeZEnv[T0]: EnvInjector1[R, T0, E, A, R1, R2, N] = new EnvInjector1(zio, lifecycle)

  def wire(
      implicit constructor: EnvConstructor.Aux[R2 with R with R1, N, T]
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
