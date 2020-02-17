package cakeless

import cakeless.ioc.builder.{ExecutableBuilder0, ModuleBuilder0}
import cakeless.nat._
import zio._

package object ioc {
  type Module[+E, +A]   = ZModule[Any, E, A]
  type UModule[+A]      = Module[Nothing, A]
  type URModule[-R, +A] = ZModule[R, Nothing, A]
  type RModule[+A]      = ZModule[Any, Throwable, A]

  def of[M <: ZModuleDecl[_, _, _]](decl: M): ModuleBuilder0[decl.R, decl.E, decl.Wired] =
    new ModuleBuilder0[decl.R, decl.E, decl.Wired](decl.asInstanceOf[ZModule[decl.R, decl.E, decl.Wired]])

  def ofExecutable[M <: ZExecutableDecl[_, _, _]](decl: M): ExecutableBuilder0[decl.R, decl.E, decl.Wired] =
    new ExecutableBuilder0[decl.R, decl.E, decl.Wired](decl.asInstanceOf[ZExecutableDecl[decl.R, decl.E, decl.Wired]])

  final val Main = new ZExecutableDecl[zio.ZEnv, Nothing, Int] {}

  implicit class ZioInject[Z[-_, +_, +_], R, E, A](private val self: Z[R, E, A]) extends AnyVal {

    /**
      * @return = a builder to inject [[R]]
      * */
    def inject0(implicit ev: Injectable[Z]): EnvInjector0[Z, R, E, A, _0, CollisionResolving.Default] = new EnvInjector0(self)

    /**
      * @return = a builder to inject [[R]] using constructor [[N]]
      * */
    def inject[N <: Nat](implicit ev: Injectable[Z]): EnvInjector0[Z, R, E, A, N, CollisionResolving.Default] = new EnvInjector0(self)
  }

  implicit class URIOInject[R, A](private val self: URIO[R, A]) extends AnyVal {

    /**
      * @return = a builder to inject [[R]]
      * */
    def inject0: EnvInjector0[ZIO, R, Nothing, A, _0, CollisionResolving.Default] = new EnvInjector0(self)

    /**
      * @return = a builder to inject [[R]] using constructor [[N]]
      * */
    def inject[N <: Nat]: EnvInjector0[ZIO, R, Nothing, A, N, CollisionResolving.Default] = new EnvInjector0(self)
  }

  implicit class URManagedInject[R, A](private val self: URManaged[R, A]) extends AnyVal {

    /**
      * @return = a builder to inject [[R]]
      * */
    def inject0: EnvInjector0[ZManaged, R, Nothing, A, _0, CollisionResolving.Default] = new EnvInjector0(self)

    /**
      * @return = a builder to inject [[R]] using constructor [[N]]
      * */
    def inject[N <: Nat]: EnvInjector0[ZManaged, R, Nothing, A, N, CollisionResolving.Default] = new EnvInjector0(self)
  }

}
