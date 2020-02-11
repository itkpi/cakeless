package cakeless

import cakeless.internal.{EnvConstructor, ZEnvLike}
import cakeless.nat.Nat
import zio._

import scala.language.implicitConversions

class EnvInjector0[Z[-_, +_, +_]: ZEnvLike, R, E, A, N <: Nat, CR <: ConflictResolution](private val z: Z[R, E, A]) {
  private val Z = implicitly[ZEnvLike[Z]]

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
  def raiseOnConflicts: EnvInjector0[Z, R, E, A, N, ConflictResolution.Raise] =
    this.asInstanceOf[EnvInjector0[Z, R, E, A, N, ConflictResolution.Raise]]

  /**
    * @return - a builder which will just warn on name collisions
    * */
  def warnConflicts: EnvInjector0[Z, R, E, A, N, ConflictResolution.Warn] =
    this.asInstanceOf[EnvInjector0[Z, R, E, A, N, ConflictResolution.Warn]]

  /**
    * @return - [[Z]] with automatically wired [[A]]
    * */
  def wire(implicit constructor: EnvConstructor.Aux[R, N, CR, Any]): Z[Any, E, A] =
    Z.provideSomeM(z)(
      ZIO.environment[Any].flatMap(constructor.construct)
    )
}

class EnvInjector1[Z[-_, +_, +_]: ZEnvLike, R, T, E, A, R1, R2, N <: Nat, CR <: ConflictResolution](
    private val z: Z[R, E, A],
    private val lifecycle: Lifecycle[R1, R2, R]
) {
  private val Z = implicitly[ZEnvLike[Z]]

  /**
    * @tparam T0 - more [[_root_.zio.ZEnv]] to be excluded from dependency resolution process
    * */
  def excludeZEnv[T0]: EnvInjector1[Z, R, T0, E, A, R1, R2, N, CR] = new EnvInjector1(z, lifecycle)

  /**
    * @return - a builder which will raise compile-time error on name collisions
    * */
  def raiseOnConflicts: EnvInjector1[Z, R, T, E, A, R1, R2, N, ConflictResolution.Raise] =
    this.asInstanceOf[EnvInjector1[Z, R, T, E, A, R1, R2, N, ConflictResolution.Raise]]

  /**
    * @return - a builder which will just warn on name collisions
    * */
  def warnConflicts: EnvInjector1[Z, R, T, E, A, R1, R2, N, ConflictResolution.Warn] =
    this.asInstanceOf[EnvInjector1[Z, R, T, E, A, R1, R2, N, ConflictResolution.Warn]]

  /**
    * @return - [[Z]] with automatically wired [[A]] (except of ZEnv part [[T]])
    * */
  def wire(
      implicit constructor: EnvConstructor.Aux[R2 with R with R1, N, CR, T]
  ): Z[T, E, A] = {
    val construct: URIO[R2 with T with R1, R2 with R with R1] = for {
      _   <- lifecycle.preStartURIO
      all <- ZIO.accessM[T](constructor.construct)
      _   <- lifecycle.postStartURIO(all)
    } yield all

    Z.provideSomeM(z)(construct)
      .asInstanceOf[Z[T, E, A]]
  }
}
