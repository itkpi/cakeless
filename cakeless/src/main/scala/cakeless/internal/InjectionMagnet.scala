package cakeless.internal

import cakeless.Lifecycle
import zio._
import scala.language.implicitConversions

trait InjectionMagnet[R, E, A] {
  type RR[R0]
  def provideSome[R0](creation: R0 => UIO[R]): ZIO[RR[R0], E, A]
}

trait LowPriorityMagnet {
  implicit def fromZio[R, E, A](zio: ZIO[R, E, A])(implicit exclude: ZEnvExcluder[R]): InjectionMagnet.Aux[exclude.Excluded, E, A, Id] = {
    new InjectionMagnet[exclude.Excluded, E, A] {
      type RR[R0] = R0
      override def provideSome[R0](creation: R0 => UIO[exclude.Excluded]): ZIO[R0, E, A] = zio.provideSomeM[R0, E](
        ZIO.environment[R0].flatMap(creation).map(exclude.construct)
      )
    }
  }
}

object InjectionMagnet extends LowPriorityMagnet {

  type Aux[R, E, A, RR0[_]] = InjectionMagnet[R, E, A] {type RR[R0] = RR0[R0]}

  implicit def fromZioWithLifecycle[R, R1, R2, E, A](
      tuple: (ZIO[R, E, A], Lifecycle[R1, R2, R])
  )(implicit exclude: ZEnvExcluder[R]): InjectionMagnet.Aux[exclude.Excluded, E, A, λ[R0 => R2 with R1 with R0]] = {
    val (zio, lf) = tuple

    new InjectionMagnet[exclude.Excluded, E, A] {
      type RR[R0] = R2 with R1 with R0
      override def provideSome[R0](creation: R0 => UIO[exclude.Excluded]): ZIO[R2 with R1 with R0, E, A] = {
        val construct: URIO[R2 with R0 with R1, R] = for {
          _ <- lf.preStartURIO
          r0 <- ZIO.environment[R0]
          c <- creation(r0)
          all = exclude.construct(c)
          _ <- lf.postStartURIO(all)
        } yield all

        construct.flatMap (all =>
          zio.provide(all)
        )
      }
    }
  }
}
