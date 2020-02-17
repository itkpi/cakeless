package cakeless.ioc.builder

import cakeless.ioc.ZModule
import zio.{ZIO, ZManaged}
import scala.annotation.unchecked.uncheckedVariance

class ModuleBuilder0[-R, +E, +A] private[cakeless] (val `decl`: ZModule[R, E, A]) extends AnyVal {
  def dependsOn[R0, E1 >: E](submodule: ZModule[R0, E1, R]): ModuleBuilder1[R0, R, E1, A] =
    new ModuleBuilder1[R0, R, E1, A](`decl`, submodule)

  def dependsOn[R0, A0, R1, A1, E1 >: E](
      submodule0: ZModule[R0, E1, A0],
      submodule1: ZModule[R1, E1, A1]
  )(implicit ev: (A0 with A1) <:< R): ModuleBuilder2[R0, R1, A0, A1, E1, A] =
    new ModuleBuilder2[R0, R1, A0, A1, E1, A](`decl`.asInstanceOf[ZModule[A0 with A1, E1, A]], submodule0, submodule1)

  def dependsOn[R0, A0, R1, A1, R2, A2, E1 >: E](
      submodule0: ZModule[R0, E1, A0],
      submodule1: ZModule[R1, E1, A1],
      submodule2: ZModule[R2, E1, A2]
  )(implicit ev: (A0 with A1 with A2) <:< R): ModuleBuilder3[R0, R1, R2, A0, A1, A2, E1, A] =
    new ModuleBuilder3[R0, R1, R2, A0, A1, A2, E1, A](
      `decl`.asInstanceOf[ZModule[A0 with A1 with A2, E1, A]],
      submodule0,
      submodule1,
      submodule2
    )

  def apply[R0 <: R, E1 >: E, A1 >: A](managed: ZManaged[R0, E1, A1]): ModuleBuilder[R0, E1, A1] =
    new ModuleBuilder(managed)

  def apply[R0 <: R, E1 >: E, A1 >: A](effect: ZIO[R0, E1, A1]): ModuleBuilder[R0, E1, A1] =
    apply(ZManaged.fromEffect(effect))

  def apply[A1 >: A](value: => A1): ModuleBuilder[R, E, A1] =
    apply(ZManaged.effectTotal(value))
}

class ModuleBuilder1[-R0, -R, +E, +A] private[cakeless] (
    val decl: ZModule[R @uncheckedVariance, E, A],
    val submodule1: ZModule[R0 @uncheckedVariance, E, R @uncheckedVariance]
) {

  def settings[R0x <: R0, Rx <: R, E1 >: E, A1 >: A](managed: ZManaged[R0x, E1, A1]): ModuleBuilder[R0x, E1, A1] =
    new ModuleBuilder(managed)

  def settings[R0x <: R0, Rx <: R, E1 >: E, A1 >: A](zio: ZIO[R0x, E1, A1]): ModuleBuilder[R0x, E1, A1] =
    new ModuleBuilder(ZManaged.fromEffect(zio))

}

class ModuleBuilder2[-R0, -R1, A0, A1, +E, +A] private[cakeless] (
    val decl: ZModule[A0 with A1, E, A],
    val submodule1: ZModule[R0, E, A0],
    val submodule2: ZModule[R1, E, A1]
) {

  def settings[R0x <: R0, R1x <: R1, E1 >: E, Ax >: A](
      managed: ZManaged[R0x with R1x, E1, Ax]
  ): ModuleBuilder[R0x with R1x, E1, Ax] =
    new ModuleBuilder(managed)

  def settings[R0x <: R0, R1x <: R1, E1 >: E, Ax >: A](
      zio: ZIO[R0x with R1x, E1, Ax]
  ): ModuleBuilder[R0x with R1x, E1, Ax] =
    new ModuleBuilder(ZManaged.fromEffect(zio))
}

class ModuleBuilder3[-R0, -R1, -R2, A0, A1, A2, +E, +A] private[cakeless] (
    val decl: ZModule[A0 with A1 with A2, E, A],
    val submodule1: ZModule[R0, E, A0],
    val submodule2: ZModule[R1, E, A1],
    val submodule3: ZModule[R2, E, A2]
) {

  def settings[R0x <: R0, R1x <: R1, R2x <: R2, E1 >: E, Ax >: A](
      managed: ZManaged[R0x with R1x with R2x, E1, Ax]
  ): ModuleBuilder[R0x with R1x with R2x, E1, Ax] =
    new ModuleBuilder(managed)

  def settings[R0x <: R0, R1x <: R1, R2x <: R2, E1 >: E, Ax >: A](
      zio: ZIO[R0x with R1x with R2x, E1, Ax]
  ): ModuleBuilder[R0x with R1x with R2x, E1, Ax] =
    new ModuleBuilder(ZManaged.fromEffect(zio))
}

class ModuleBuilder[-R, +E, +A] private[cakeless] (val `env`: ZManaged[R, E, A]) extends AnyVal
