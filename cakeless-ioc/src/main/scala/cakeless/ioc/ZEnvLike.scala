package cakeless.ioc

import zio.{NeedsEnv, ZIO, ZManaged}
import scala.annotation.implicitNotFound

@implicitNotFound("""Type ${Z} is not like ZIO or ZManaged to support automatic wiring.
   Please provide your custom instance if you're sure.""")
trait ZEnvLike[Z[-_, +_, +_]] {
  def provideSomeM[R, E, A, R0, E1 >: E](z: Z[R, E, A])(r0: ZIO[R0, E1, R])(implicit ev: NeedsEnv[R]): Z[R0, E1, A]
  def liftEffect[R, E, A](zio: ZIO[R, E, A]): Z[R, E, A]
  def flatMap[R, E, A, R1 <: R, E1 >: E, B](z: Z[R, E, A])(f: A => Z[R1, E1, B]): Z[R1, E1, B]
}

object ZEnvLike {
  implicit val zioInstance: ZEnvLike[ZIO] = new ZEnvLike[ZIO] {
    override def provideSomeM[R, E, A, R0, E1 >: E](z: ZIO[R, E, A])(r0: ZIO[R0, E1, R])(implicit ev: NeedsEnv[R]): ZIO[R0, E1, A] =
      z.provideSomeM(r0)

    override def liftEffect[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] = zio

    override def flatMap[R, E, A, R1 <: R, E1 >: E, B](z: ZIO[R, E, A])(f: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
      z.flatMap(f)
  }

  implicit val zmanagedInstance: ZEnvLike[ZManaged] = new ZEnvLike[ZManaged] {
    override def provideSomeM[R, E, A, R0, E1 >: E](
        z: ZManaged[R, E, A]
    )(r0: ZIO[R0, E1, R])(implicit ev: NeedsEnv[R]): ZManaged[R0, E1, A] =
      ZManaged.fromEffect(r0).flatMap(z.provide)

    override def liftEffect[R, E, A](zio: ZIO[R, E, A]): ZManaged[R, E, A] = ZManaged.fromEffect(zio)

    override def flatMap[R, E, A, R1 <: R, E1 >: E, B](z: ZManaged[R, E, A])(f: A => ZManaged[R1, E1, B]): ZManaged[R1, E1, B] =
      z.flatMap(f)
  }
}
