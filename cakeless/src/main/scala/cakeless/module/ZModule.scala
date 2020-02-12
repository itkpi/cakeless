package cakeless.module

import cakeless.internal.ZEnvLike
import zio.{Managed, NeedsEnv, ZIO, ZManaged}

import scala.annotation.unchecked.uncheckedVariance

sealed trait ZModule[-R, +E, +A] {
  def flatMap[R1 <: R, E1 >: E, B](f: A => ZModule[R1, E1, B]): ZModule[R1, E1, B]

  def provideSomeM[R0, E1 >: E](r0: ZIO[R0, E1, R])(implicit ev: NeedsEnv[R]): ZModule[R0, E1, A]

  def map[B](f: A => B): ZModule[R, E, B] = ZModule.MapFn(this, f)

  def environment: ZManaged[R, E, A] = ZModule.toZManaged(this)
}

trait ModuleDecl[+E0, +A0] extends ZModule[Any, E0, A0] {
  final type E     = E0 @uncheckedVariance
  final type Wired = A0 @uncheckedVariance
  final type Decl  = this.type

  override final def flatMap[R1 <: Any, E1 >: E0, B](f: A0 => ZModule[R1, E1, B]): ZModule[R1, E1, B] =
    ZModule.FlatMapFn(this, f)

  override final def provideSomeM[R0, E1 >: E0](r0: ZIO[R0, E1, Any])(implicit ev: NeedsEnv[Any]): ZModule[R0, E1, A0] =
    new ZModule.Managed(ZManaged.fromEffect(r0).flatMap(environment.provide))

}

class ModuleBuilder[E, A](val `env`: Managed[E, A]) extends AnyVal

class ModuleDefn[E, A](builder: ModuleBuilder[E, A]) extends ModuleDecl[E, A] {
  final override def environment: Managed[E, A] = builder.`env`
}

object Limits extends ModuleDecl[Throwable, Int]
object LimitsImpl
    extends ModuleDefn(
      of(Limits) {
        ZManaged.fromEffect {
          ZIO(1)
        }
      }
    )

object Foo {
  val x: Module[Throwable, Int] = LimitsImpl.map(_ + 1)
}

object ZModule {

  def succeed[A](value: A): UModule[A]                                   = ZModule.Succeed(value)
  def effectTotal[A](effect: => A): UModule[A]                           = ZModule.Effect(ZIO.effectTotal(effect))
  def fail[E](error: E): Module[E, Nothing]                              = ZModule.Fail(error)
  def fromEffect[R, E, A](zio: ZIO[R, E, A]): ZModule[R, E, A]           = ZModule.Effect(zio)
  def environment[R]: URModule[R, R]                                     = ZModule.Effect(ZIO.environment[R])
  def fromManaged[R, E, A](managed: ZManaged[R, E, A]): ZModule[R, E, A] = new ZModule.Managed(managed)

  private[cakeless] def toZManaged[R, E, A](module: ZModule[R, E, A]): ZManaged[R, E, A] = module match {
    case decl: ModuleDecl[E, A]            => decl.environment
    case ZModule.Succeed(value)            => ZManaged.succeed(value)
    case ZModule.Fail(e)                   => ZManaged.fail(e)
    case ZModule.Effect(zio)               => ZManaged.fromEffect(zio)
    case m: ZModule.Managed[R, E, A]       => m.zmanaged
    case mapFn: ZModule.MapFn[R, E, a0, A] => toZManaged(mapFn.parent).map(mapFn.f.asInstanceOf[Any => A])
    case flatMapFn: ZModule.FlatMapFn[r0, R, e0, E, a0, A] =>
      val managedParent = toZManaged(flatMapFn.parent)
      managedParent
        .flatMap { a0 =>
          toZManaged {
            flatMapFn.f.asInstanceOf[Any => ZModule[R, E, A]].apply(a0)
          }
        }
        .asInstanceOf[ZManaged[R, E, A]]
  }

  implicit val zmoduleLikeZEnv: ZEnvLike[ZModule] = new ZEnvLike[ZModule] {
    override def provideSomeM[R, E, A, R0, E1 >: E](
        z: ZModule[R, E, A]
    )(r0: ZIO[R0, E1, R])(implicit ev: NeedsEnv[R]): ZModule[R0, E1, A] =
      z provideSomeM r0

    override def liftEffect[R, E, A](zio: ZIO[R, E, A]): ZModule[R, E, A] = ZModule.Effect(zio)

    override def flatMap[R, E, A, R1 <: R, E1 >: E, B](
        z: ZModule[R, E, A]
    )(f: A => ZModule[R1, E1, B]): ZModule[R1, E1, B] =
      z flatMap f
  }

  private[cakeless] case class Succeed[A](value: A) extends ZModule[Any, Nothing, A] {
    override def flatMap[R1 <: Any, E1 >: Nothing, B](f: A => ZModule[R1, E1, B]): ZModule[R1, E1, B] =
      f(value)

    override def provideSomeM[R0, E1 >: Nothing](r0: ZIO[R0, E1, Any])(implicit ev: NeedsEnv[Any]): ZModule[R0, E1, A] = this
  }

  private[cakeless] case class Fail[E](failure: E) extends ZModule[Any, E, Nothing] {
    override def flatMap[R1 <: Any, E1 >: E, B](f: Nothing => ZModule[R1, E1, B]): ZModule[R1, E1, B]                  = this
    override def provideSomeM[R0, E1 >: E](r0: ZIO[R0, E1, Any])(implicit ev: NeedsEnv[Any]): ZModule[R0, E1, Nothing] = this
  }

  private[cakeless] case class Effect[R, E, A](effect: ZIO[R, E, A]) extends ZModule[R, E, A] {
    override def flatMap[R1 <: R, E1 >: E, B](f: A => ZModule[R1, E1, B]): ZModule[R1, E1, B] =
      ZModule.FlatMapFn(this, f)

    override def provideSomeM[R0, E1 >: E](r0: ZIO[R0, E1, R])(implicit ev: NeedsEnv[R]): ZModule[R0, E1, A] =
      ZModule.Effect(effect.provideSomeM(r0))
  }

  private[cakeless] class Managed[R, E, A](val zmanaged: ZManaged[R, E, A]) extends ZModule[R, E, A] {
    override def flatMap[R1 <: R, E1 >: E, B](f: A => ZModule[R1, E1, B]): ZModule[R1, E1, B] =
      ZModule.FlatMapFn(this, f)

    override def provideSomeM[R0, E1 >: E](r0: ZIO[R0, E1, R])(implicit ev: NeedsEnv[R]): ZModule[R0, E1, A] =
      new ZModule.Managed(ZManaged.fromEffect(r0).flatMap(zmanaged.provide))
  }

  private[cakeless] case class MapFn[R, E, A, B](parent: ZModule[R, E, A], f: A => B) extends ZModule[R, E, B] {
    override def flatMap[R1 <: R, E1 >: E, C](f2: B => ZModule[R1, E1, C]): ZModule[R1, E1, C] =
      ZModule.FlatMapFn(parent, f andThen f2)

    override def provideSomeM[R0, E1 >: E](r0: ZIO[R0, E1, R])(implicit ev: NeedsEnv[R]): ZModule[R0, E1, B] =
      ZModule.MapFn(parent.provideSomeM(r0), f)
  }

  private[cakeless] case class FlatMapFn[R, R1 <: R, E, E1 >: E, A, B](parent: ZModule[R, E, A], f: A => ZModule[R1, E1, B])
      extends ZModule[R1, E1, B] {

    override def flatMap[R2 <: R1, E2 >: E1, C](f2: B => ZModule[R2, E2, C]): ZModule[R2, E2, C] =
      ZModule.FlatMapFn[R, R2, E, E2, A, C](parent, f(_).flatMap(f2))

    override def provideSomeM[R0, E2 >: E1](r0: ZIO[R0, E2, R1])(implicit ev: NeedsEnv[R1]): ZModule[R0, E2, B] =
      ZModule.FlatMapFn(parent, f(_: A).provideSomeM[R0, E2](r0)).asInstanceOf[ZModule[R0, E2, B]]
  }
}
