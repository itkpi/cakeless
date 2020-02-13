package cakeless.module

import zio.{ZIO, ZManaged}

class ModuleBuilder0[R, E, A] private[cakeless] (val `decl`: ZModule[R, E, A]) extends AnyVal {
  def dependsOn[R0, E1 >: E](submodule: ZModule[R0, E1, R]): ModuleBuilder1[R0, R, E1, A] =
    new ModuleBuilder1[R0, R, E1, A](`decl`, submodule)

  def dependsOn[R0, A0, R1, A1, E1 >: E](
      submodule0: ZModule[R0, E1, A0],
      submodule1: ZModule[R1, E1, A1]
  )(implicit ev: (A0 with A1) <:< R): ModuleBuilder2[R0, R1, A0, A1, E1, A] =
    new ModuleBuilder2[R0, R1, A0, A1, E1, A](`decl`.asInstanceOf[ZModule[A0 with A1, E1, A]], submodule0, submodule1)

  def apply(managed: ZManaged[R, E, A]): ModuleBuilder[R, E, A] = new ModuleBuilder(managed)
  def apply(effect: ZIO[R, E, A]): ModuleBuilder[R, E, A]       = apply(ZManaged.fromEffect(effect))
}

class ModuleBuilder1[R0, R, E, A] private[cakeless] (
    val decl: ZModule[R, E, A],
    val submodule1: ZModule[R0, E, R]
) {

  def settings(managed: ZManaged[R0, E, A]): ModuleBuilder[R0, E, A] = new ModuleBuilder(managed)
  def settings(zio: ZIO[R0, E, A]): ModuleBuilder[R0, E, A]          = settings(ZManaged.fromEffect(zio))
}

class ModuleBuilder2[R0, R1, A0, A1, E, A] private[cakeless] (
    val decl: ZModule[A0 with A1, E, A],
    val submodule1: ZModule[R0, E, A0],
    val submodule2: ZModule[R1, E, A1]
) {

  def settings(managed: ZManaged[R0 with R1, E, A]): ModuleBuilder[R0 with R1, E, A] = new ModuleBuilder(managed)
  def settings(zio: ZIO[R0 with R1, E, A]): ModuleBuilder[R0 with R1, E, A]          = settings(ZManaged.fromEffect(zio))
}

class ModuleBuilder[-R, +E, +A] private[cakeless] (val `env`: ZManaged[R, E, A]) extends AnyVal
