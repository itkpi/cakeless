package cakeless.ioc

import cakeless.ioc.builder.ModuleBuilder
import zio.{ZIO, ZManaged}

sealed trait ZModule[-R, +E, +A]

trait ZModuleDecl[-Rx, +Ex, +Ax] extends ZModule[Rx, Ex, Ax] {
  type R >: Rx
  type E <: Ex
  type Wired <: Ax
  final type Decl = this.type
}

trait ModuleDecl[+Ex, +Ax]   extends ZModuleDecl[Any, Ex, Ax]
trait RModuleDecl[-Rx, +Ax]  extends ZModuleDecl[Rx, Throwable, Ax]
trait UModuleDecl[+Ax]       extends ZModuleDecl[Any, Nothing, Ax]
trait URModuleDecl[-Rx, +Ax] extends ZModuleDecl[Rx, Nothing, Ax]

sealed trait ZModuleAsManaged[-Rx, +Ex, +Ax] extends ZModule[Rx, Ex, Ax] {
  def asManaged: ZManaged[Rx, Ex, Ax]
}
class ModuleDefn[-Rx, +Ex, +Ax](builder: ModuleBuilder[Rx, Ex, Ax]) extends ZModuleAsManaged[Rx, Ex, Ax] {
  final override def asManaged: ZManaged[Rx, Ex, Ax] = builder.`env`
}

object ZModule {

  @`inline` def succeed[A](value: A): UModule[A]                                   = fromManaged(ZManaged.succeed(value))
  @`inline` def effectTotal[A](effect: => A): UModule[A]                           = fromManaged(ZManaged.effectTotal(effect))
  @`inline` def fail[E](error: E): Module[E, Nothing]                              = fromManaged(ZManaged.fail(error))
  @`inline` def fromEffect[R, E, A](zio: ZIO[R, E, A]): ZModule[R, E, A]           = fromManaged(ZManaged.fromEffect(zio))
  @`inline` def environment[R]: URModule[R, R]                                     = fromManaged(ZManaged.environment[R])
  @`inline` def fromManaged[R, E, A](managed: ZManaged[R, E, A]): ZModule[R, E, A] = new WrappedZManaged(managed)

  private[cakeless] class WrappedZManaged[-R, +E, +A](override val asManaged: ZManaged[R, E, A]) extends ZModuleAsManaged[R, E, A]
}
