package cakeless.ioc.builder

import cakeless.ioc.{ModuleDefn, ZExecutable}
import zio.{ZIO, ZManaged}

import scala.annotation.unchecked.uncheckedVariance

class ZExecutableBuilderInitial[-R, +E, +A] private[cakeless] (
    decl: ZExecutable[R, E, A]
) {
  def ignoreArgs: ZExecutableBuilder0[R, E, A, ZUsage.NoUsage] =
    new ZExecutableBuilder0[R, E, A, ZUsage.NoUsage](decl, ZUsage.noUsage[R, E, A])

  def withArgs[R0 <: R, E1 >: E, A1 >: A](
      f: (
          List[String],
          ZManaged[R0, E1, A1]
      ) => ZIO[R0, E1, A1]
  ): ZExecutableBuilder0[R0, E1, A1, ZUsage.Using] =
    new ZExecutableBuilder0[R0, E1, A1, ZUsage.Using](decl, ZUsage.using[R0, E1, A1](f))
}

class ZExecutableBuilder0[-R, +E, +A, Usage[-r, +e, +a] <: ZUsage[r, e, a]](
    decl: ZExecutable[R, E, A],
    usage: ZUsage.Aux[R, E, A, Usage]
) {
  def aggregate[R0 <: R, E1 >: E](submodule: ModuleDefn[R0, E1, R]): ZExecutableBuilder1[R0, R, E1, A, Usage] =
    new ZExecutableBuilder1[R0, R, E1, A, Usage](
      decl,
      submodule,
      usage
    )

  def aggregate[R0 <: R, A0, R1 <: R, A1, E1 >: E](
      submodule0: ModuleDefn[R0, E1, A0],
      submodule1: ModuleDefn[R1, E1, A1]
  )(implicit ev: (A0 with A1) <| R): ZExecutableBuilder2[R0, R1, A0, A1, E1, A, Usage] =
    new ZExecutableBuilder2[R0, R1, A0, A1, E1, A, Usage](
      decl.asInstanceOf[ZExecutable[A0 with A1, E1, A]],
      submodule0,
      submodule1,
      usage.widenR[A0 with A1]
    )(
      R0 = Liskov.antisim(ev),
      R1 = Liskov.antisim(ev)
    )

  def aggregate[R0 <: R, A0, R1, A1, R2, A2, E1 >: E](
      submodule0: ModuleDefn[R0, E1, A0],
      submodule1: ModuleDefn[R1, E1, A1],
      submodule2: ModuleDefn[R2, E1, A2]
  )(implicit ev: (A0 with A1 with A2) <| R): ZExecutableBuilder3[R0, R1, R2, A0, A1, A2, E1, A, Usage] =
    new ZExecutableBuilder3[R0, R1, R2, A0, A1, A2, E1, A, Usage](
      decl.asInstanceOf[ZExecutable[A0 with A1 with A2, E1, A]],
      submodule0,
      submodule1,
      submodule2,
      usage.widenAllEv[A0 with A1 with A2, E1, A]
    )(
      R0 = Liskov.antisim(ev),
      R1 = Liskov.antisim(ev),
      R2 = Liskov.antisim(ev)
    )

  def apply[R0 <: R, E1 >: E, A1 >: A](managed: ZManaged[R0, E1, A1]): ZExecutableBuilder[R0, E1, A1, Usage] =
    new ZExecutableBuilder[R0, E1, A1, Usage](managed, usage)

  def apply[R0 <: R, E1 >: E, A1 >: A](effect: ZIO[R0, E1, A1]): ZExecutableBuilder[R0, E1, A1, Usage] =
    apply(ZManaged.fromEffect(effect))

  def apply[A1 >: A](value: => A1): ZExecutableBuilder[R, E, A1, Usage] =
    apply(ZManaged.effectTotal(value))
}

class ZExecutableBuilder1[-R0, -R >: R0, +E, +A, Usage[-r, +e, +a] <: ZUsage[r, e, a]] private[cakeless] (
    decl: ZExecutable[R, E, A],
    submodule1: ModuleDefn[R0, E, R],
    usage: ZUsage.Aux[R, E, A, Usage]
) {

  def settings[R0x <: R0, Rx <: R, E1 >: E, A1 >: A](managed: ZManaged[R0x, E1, A1]): ZExecutableBuilder[R0x, E1, A1, Usage] =
    new ZExecutableBuilder[R0x, E1, A1, Usage](managed, usage)

  def settings[R0x <: R0, Rx <: R, E1 >: E, A1 >: A](zio: ZIO[R0x, E1, A1]): ZExecutableBuilder[R0x, E1, A1, Usage] =
    new ZExecutableBuilder[R0x, E1, A1, Usage](ZManaged.fromEffect(zio), usage.widenAllSubt[R0x, E1, A1])

}

class ZExecutableBuilder2[-R0, -R1, A0, A1, +E, +A, Usage[-r, +e, +a] <: ZUsage[r, e, a]] private[cakeless] (
    decl: ZExecutable[A0 with A1, E, A],
    submodule1: ModuleDefn[R0, E, A0],
    submodule2: ModuleDefn[R1, E, A1],
    usage: ZUsage.Aux[A0 with A1, E, A, Usage]
)(implicit R0: R0 <| (A0 with A1), R1: R1 <| (A0 with A1)) {

  def settings[R0x <: R0, R1x <: R1, E1 >: E, Ax >: A](
      managed: ZManaged[R0x with R1x, E1, Ax]
  ): ZExecutableBuilder[R0x with R1x, E1, Ax, Usage] =
    new ZExecutableBuilder[R0x with R1x, E1, Ax, Usage](managed, usage.widenAllEv[R0x with R1x, E1, Ax](R0))

  def settings[R0x <: R0, R1x <: R1, E1 >: E, Ax >: A](
      zio: ZIO[R0x with R1x, E1, Ax]
  ): ZExecutableBuilder[R0x with R1x, E1, Ax, Usage] =
    new ZExecutableBuilder[R0x with R1x, E1, Ax, Usage](ZManaged.fromEffect(zio), usage.widenAllEv[R0x with R1x, E1, Ax](R0))
}

class ZExecutableBuilder3[-R0, -R1, -R2, A0, A1, A2, +E, +A, Usage[-r, +e, +a] <: ZUsage[r, e, a]] private[cakeless] (
    decl: ZExecutable[A0 with A1 with A2, E, A],
    submodule1: ModuleDefn[R0, E, A0],
    submodule2: ModuleDefn[R1, E, A1],
    submodule3: ModuleDefn[R2, E, A2],
    usage: ZUsage.Aux[A0 with A1 with A2, E, A, Usage]
)(implicit R0: R0 <| (A0 with A1 with A2), R1: R1 <| (A0 with A1 with A2), R2: R2 <| (A0 with A1 with A2)) {

  def settings[R0x <: R0, R1x <: R1, R2x <: R2, E1 >: E, Ax >: A](
      managed: ZManaged[R0x with R1x with R2x, E1, Ax]
  ): ZExecutableBuilder[R0x with R1x with R2x, E1, Ax, Usage] =
    new ZExecutableBuilder[R0x with R1x with R2x, E1, Ax, Usage](
      managed,
      usage.widenAllEv[R0x with R1x with R2x, E1, Ax](R0)
    )

  def settings[R0x <: R0, R1x <: R1, R2x <: R2, E1 >: E, Ax >: A](
      zio: ZIO[R0x with R1x with R2x, E1, Ax]
  ): ZExecutableBuilder[R0x with R1x with R2x, E1, Ax, Usage] =
    new ZExecutableBuilder[R0x with R1x with R2x, E1, Ax, Usage](
      ZManaged.fromEffect(zio),
      usage.widenAllEv[R0x with R1x with R2x, E1, Ax](R0)
    )
}

class ZExecutableBuilder[-R, +E, +A, +Usage[-r, +e, +a] <: ZUsage[r, e, a]] private[cakeless] (
    selfManaged: ZManaged[R, E, A],
    usage: ZUsage.Aux[R, E, A, Usage]
) {

  private[cakeless] def getManaged[R0 <: R, E1 >: E, A1 >: A]: ZManaged[R0, E1, A1]        = selfManaged
  private[cakeless] def getUsage[R0 <: R, E1 >: E, A1 >: A]: ZUsage.Aux[R0, E1, A1, Usage] = usage
  def exitM(
      error: E => ZIO[zio.ZEnv, Nothing, Int],
      succeed: A => ZIO[zio.ZEnv, Nothing, Int]
  )(implicit ev: R |> zio.ZEnv): ZMainBuilder[E, A, Usage] = new ZMainBuilder[E, A, Usage](
    selfManaged.asInstanceOf[ZManaged[zio.ZEnv, E, A]],
    usage.widenR[zio.ZEnv],
    error,
    succeed
  )
}

class ZMainBuilder[+E, +A, +Usage[-r, +e, +a] <: ZUsage[r, e, a]] private[cakeless] (
    val getManaged: ZManaged[zio.ZEnv, E, A],
    val getUsage: ZUsage.Aux[zio.ZEnv, E, A, Usage],
    error: E => ZIO[zio.ZEnv, Nothing, Int],
    succeed: A => ZIO[zio.ZEnv, Nothing, Int]
) {
  private[cakeless] def failure[E1 >: E](e: E1): ZIO[zio.ZEnv, Nothing, Int] = error(e.asInstanceOf[E])
  private[cakeless] def success[A1 >: A](a: A1): ZIO[zio.ZEnv, Nothing, Int] = succeed(a.asInstanceOf[A])
}
