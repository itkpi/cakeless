import zio._
import cakeless.nat._
import scala.language.experimental.macros

package object cakeless {
  implicit class ZioInject[R, E, A](private val self: ZIO[R, E, A]) extends AnyVal {
    def injectPrimary: EnvInjector0[R, E, A, _0]   = new EnvInjector0(self)
    def inject[N <: Nat]: EnvInjector0[R, E, A, N] = new EnvInjector0(self)
  }

  implicit class URIOInject[R, A](private val self: URIO[R, A]) extends AnyVal {
    def injectPrimary: EnvInjector0[R, Nothing, A, _0]   = new EnvInjector0(self)
    def inject[N <: Nat]: EnvInjector0[R, Nothing, A, N] = new EnvInjector0(self)
  }
}
