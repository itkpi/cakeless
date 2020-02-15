package cakeless.ioc

import zio._
import cakeless._
import cakeless.nat._
import cakeless.kernel.EnvConstructor
import scala.language.implicitConversions

class EnvInjector0[Z[-_, +_, +_]: Injectable, R, E, A, N <: Nat, CR <: CollisionResolving](private val z: Z[R, E, A]) {
  private val Z = implicitly[Injectable[Z]]

  /**
    * @param lifecycle - lifecycle for component [[A]]
    * @return - builder with {{{lifecycle}}} provided
    * */
  def withLifecycle[R1, R2](lifecycle: Lifecycle[R1, R2, R]): EnvInjector1[Z, R, Any, E, A, R1, R2, N, CR] =
    new EnvInjector1[Z, R, Any, E, A, R1, R2, N, CR](z, lifecycle)

  /**
    * @tparam T - some [[_root_.zio.ZEnv]] to be excluded from dependency resolution process
    * */
  def excludeZEnv[T]: EnvInjector1[Z, _ >: R, T, E, A, Any, Any, N, CR] = new EnvInjector1(z, Lifecycle.empty)

  /**
    * @return - a builder which will raise compile-time error on name collisions
    * */
  def resolveCollisions[CR0 <: CollisionResolving]: EnvInjector0[Z, R, E, A, N, CR0] =
    this.asInstanceOf[EnvInjector0[Z, R, E, A, N, CR0]]

  /**
    * @return - [[Z]] with automatically wired [[A]]
    * */
  def wire(implicit constructor: EnvConstructor.Aux[R, N, CR, Any]): Z[Any, E, A] =
    Z.provideSomeM(z)(
      ZIO.environment[Any].map(constructor.construct)
    )
}

class EnvInjector1[Z[-_, +_, +_]: Injectable, R, T, E, A, R1, R2, N <: Nat, CR <: CollisionResolving](
    private val z: Z[R, E, A],
    private val lifecycle: Lifecycle[R1, R2, R]
) {
  private val Z = implicitly[Injectable[Z]]

  /**
    * @tparam T0 - more [[_root_.zio.ZEnv]] to be excluded from dependency resolution process
    * */
  def excludeZEnv[T0]: EnvInjector1[Z, R, T0, E, A, R1, R2, N, CR] = new EnvInjector1(z, lifecycle)

  /**
    * @return - a builder which will raise compile-time error on name collisions
    * */
  def resolveCollisions[CR0 <: CollisionResolving]: EnvInjector1[Z, R, T, E, A, R1, R2, N, CR0] =
    this.asInstanceOf[EnvInjector1[Z, R, T, E, A, R1, R2, N, CR0]]

  /**
    * @return - [[Z]] with automatically wired [[A]] (except of ZEnv part [[T]])
    * */
  def wire(
      implicit constructor: EnvConstructor.Aux[R2 with R with R1, N, CR, T]
  ): Z[T, E, A] = {
    val construct: URIO[R2 with T with R1, R2 with R with R1] = for {
      _   <- lifecycle.preStartURIO
      all <- ZIO.access[T](constructor.construct)
      _   <- lifecycle.postStartURIO(all)
    } yield all

    Z.provideSomeM(z)(construct)
      .asInstanceOf[Z[T, E, A]]
  }
}
