import zio._
import cakeless.nat._
import scala.language.experimental.macros

package object cakeless {
  implicit class ZioInject[R, E, A](private val self: ZIO[R, E, A]) extends AnyVal {

    /**
      * @return = a builder to inject [[R]]
      * */
    def injectPrimary: EnvInjector0[R, E, A, _0, ConflictResolution.Raise] = new EnvInjector0(self)

    /**
      * @return = a builder to inject [[R]] using constructor [[N]]
      * */
    def inject[N <: Nat]: EnvInjector0[R, E, A, N, ConflictResolution.Raise] = new EnvInjector0(self)
  }

  implicit class URIOInject[R, A](private val self: URIO[R, A]) extends AnyVal {

    /**
      * @return = a builder to inject [[R]]
      * */
    def injectPrimary: EnvInjector0[R, Nothing, A, _0, ConflictResolution.Raise] = new EnvInjector0(self)

    /**
      * @return = a builder to inject [[R]] using constructor [[N]]
      * */
    def inject[N <: Nat]: EnvInjector0[R, Nothing, A, N, ConflictResolution.Raise] = new EnvInjector0(self)
  }
}
