import cakeless.internal.ZEnvLike
import zio._
import cakeless.nat._
import scala.language.experimental.macros

package object cakeless {
  implicit class ZioInject[Z[-_, +_, +_], R, E, A](private val self: Z[R, E, A]) extends AnyVal {

    /**
      * @return = a builder to inject [[R]]
      * */
    def inject0(implicit ev: ZEnvLike[Z]): EnvInjector0[Z, R, E, A, _0, ConflictResolution.Auto] = new EnvInjector0(self)

    /**
      * @return = a builder to inject [[R]] using constructor [[N]]
      * */
    def inject[N <: Nat](implicit ev: ZEnvLike[Z]): EnvInjector0[Z, R, E, A, N, ConflictResolution.Auto] = new EnvInjector0(self)
  }

  implicit class URIOInject[R, A](private val self: URIO[R, A]) extends AnyVal {

    /**
      * @return = a builder to inject [[R]]
      * */
    def inject0: EnvInjector0[ZIO, R, Nothing, A, _0, ConflictResolution.Auto] = new EnvInjector0(self)

    /**
      * @return = a builder to inject [[R]] using constructor [[N]]
      * */
    def inject[N <: Nat]: EnvInjector0[ZIO, R, Nothing, A, N, ConflictResolution.Auto] = new EnvInjector0(self)
  }

  implicit class URManagedInject[R, A](private val self: URManaged[R, A]) extends AnyVal {

    /**
      * @return = a builder to inject [[R]]
      * */
    def inject0: EnvInjector0[ZManaged, R, Nothing, A, _0, ConflictResolution.Raise] = new EnvInjector0(self)

    /**
      * @return = a builder to inject [[R]] using constructor [[N]]
      * */
    def inject[N <: Nat]: EnvInjector0[ZManaged, R, Nothing, A, N, ConflictResolution.Raise] = new EnvInjector0(self)
  }
}
