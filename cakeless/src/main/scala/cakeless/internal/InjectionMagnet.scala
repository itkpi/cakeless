package cakeless.internal

import cakeless.Lifecycle
import zio._
import scala.language.implicitConversions

trait InjectionMagnet[R, E, A] {
  type ProvideRes <: ZIO[Nothing, E, A]
  type ProvideSomeRes[R0] >: ZIO[R0, E, A]
  def provide(creation: UIO[R]): ProvideRes
  def provideSome[R0](creation: R0 => UIO[R]): ProvideSomeRes[R0]
}

trait LowPriorityMagnet {
  implicit def fromZio[R, E, A](zio: ZIO[R, E, A]): InjectionMagnet.Aux[R, E, A, IO[E, A], ZIO[*, E, A]] = new InjectionMagnet[R, E, A] {
    override type ProvideRes        = IO[E, A]
    override type ProvideSomeRes[x] = ZIO[x, E, A]
    override def provide(creation: UIO[R]): ProvideRes = zio.provideM(
      ZIO.environment[R].provideM(creation)
    )
    override def provideSome[R0](creation: R0 => UIO[R]): ProvideSomeRes[R0] = zio.provideSomeM(ZIO.environment[R0].flatMap(creation))
  }
}

object InjectionMagnet extends LowPriorityMagnet {
  type Aux[R, E, A, Res0 <: ZIO[Nothing, E, A], Res1[x] >: ZIO[x, E, A]] = InjectionMagnet[R, E, A] {
    type ProvideRes = Res0; type ProvideSomeRes[x] = Res1[x]
  }

  implicit def fromZioWithLifecycle[R, R1, R2, E, A](
      tuple: (ZIO[R, E, A], Lifecycle[R1, R2, R])
  ): InjectionMagnet.Aux[R2 with R with R1, E, A, ZIO[R2 with R1, E, A], λ[x => ZIO[R2 with R1 with x, E, A]]] = {
    val (zio, lf) = tuple

    new InjectionMagnet[R2 with R with R1, E, A] {
      override type ProvideRes        = ZIO[R2 with R1, E, A]
      override type ProvideSomeRes[x] = ZIO[R2 with R1 with x, E, A]
      override def provide(creation: UIO[R2 with R with R1]): ProvideRes =
        for {
          _   <- lf.preStartURIO
          c   <- creation
          res <- zio.provide(c)
          _   <- lf.postStartURIO(c)
        } yield res

      override def provideSome[R0](creation: R0 => UIO[R2 with R with R1]): ProvideSomeRes[R0] =
        for {
          r0  <- ZIO.environment[R0]
          _   <- lf.preStartURIO
          c   <- creation(r0)
          res <- zio.provide(c)
          _   <- lf.postStartURIO(c)
        } yield res
    }
  }
}
