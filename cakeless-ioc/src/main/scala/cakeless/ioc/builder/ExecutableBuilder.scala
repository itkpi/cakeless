package cakeless.ioc.builder

import cakeless.ioc.{ModuleDefn, ZExecutable}
import zio.{ZIO, ZManaged}
import scala.annotation.unchecked.uncheckedVariance

class ExecutableBuilder0[-R, +E, +A] private[cakeless] (val `decl`: ZExecutable[R, E, A]) extends AnyVal {
  def aggregate[R0, E1 >: E](submodule: ModuleDefn[R0, E1, R]): ExecutableBuilder1[R0, R, E1, A] =
    new ExecutableBuilder1[R0, R, E1, A](`decl`, submodule)

  def aggregate[R0, A0, R1, A1, E1 >: E](
      submodule0: ModuleDefn[R0, E1, A0],
      submodule1: ModuleDefn[R1, E1, A1]
  )(implicit ev: (A0 with A1) <:< R): ExecutableBuilder2[R0, R1, A0, A1, E1, A] =
    new ExecutableBuilder2[R0, R1, A0, A1, E1, A](`decl`.asInstanceOf[ZExecutable[A0 with A1, E1, A]], submodule0, submodule1)

  def aggregate[R0, A0, R1, A1, R2, A2, E1 >: E](
      submodule0: ModuleDefn[R0, E1, A0],
      submodule1: ModuleDefn[R1, E1, A1],
      submodule2: ModuleDefn[R2, E1, A2]
  )(implicit ev: (A0 with A1 with A2) <:< R): ExecutableBuilder3[R0, R1, R2, A0, A1, A2, E1, A] =
    new ExecutableBuilder3[R0, R1, R2, A0, A1, A2, E1, A](
      `decl`.asInstanceOf[ZExecutable[A0 with A1 with A2, E1, A]],
      submodule0,
      submodule1,
      submodule2
    )

  def apply[R0 <: R, E1 >: E, A1 >: A](managed: ZManaged[R0, E1, A1]): ExecutableWithoutUsage[R0, E1, A1] =
    new ExecutableWithoutUsage(managed)

  def apply[R0 <: R, E1 >: E, A1 >: A](effect: ZIO[R0, E1, A1]): ExecutableWithoutUsage[R0, E1, A1] =
    apply(ZManaged.fromEffect(effect))

  def apply[A1 >: A](value: => A1): ExecutableWithoutUsage[R, E, A1] =
    apply(ZManaged.effectTotal(value))
}

class ExecutableBuilder1[-R0, -R, +E, +A] private[cakeless] (
    val decl: ZExecutable[R @uncheckedVariance, E, A],
    val submodule1: ModuleDefn[R0 @uncheckedVariance, E, R @uncheckedVariance]
) {

  def settings[R0x <: R0, Rx <: R, E1 >: E, A1 >: A](managed: ZManaged[R0x, E1, A1]): ExecutableWithoutUsage[R0x, E1, A1] =
    new ExecutableWithoutUsage(managed)

  def settings[R0x <: R0, Rx <: R, E1 >: E, A1 >: A](zio: ZIO[R0x, E1, A1]): ExecutableWithoutUsage[R0x, E1, A1] =
    new ExecutableWithoutUsage(ZManaged.fromEffect(zio))

}

class ExecutableBuilder2[-R0, -R1, A0, A1, +E, +A] private[cakeless] (
    val decl: ZExecutable[A0 with A1, E, A],
    val submodule1: ModuleDefn[R0, E, A0],
    val submodule2: ModuleDefn[R1, E, A1]
) {

  def settings[R0x <: R0, R1x <: R1, E1 >: E, Ax >: A](
      managed: ZManaged[R0x with R1x, E1, Ax]
  ): ExecutableWithoutUsage[R0x with R1x, E1, Ax] =
    new ExecutableWithoutUsage(managed)

  def settings[R0x <: R0, R1x <: R1, E1 >: E, Ax >: A](
      zio: ZIO[R0x with R1x, E1, Ax]
  ): ExecutableWithoutUsage[R0x with R1x, E1, Ax] =
    new ExecutableWithoutUsage(ZManaged.fromEffect(zio))
}

class ExecutableBuilder3[-R0, -R1, -R2, A0, A1, A2, +E, +A] private[cakeless] (
    val decl: ZExecutable[A0 with A1 with A2, E, A],
    val submodule1: ModuleDefn[R0, E, A0],
    val submodule2: ModuleDefn[R1, E, A1],
    val submodule3: ModuleDefn[R2, E, A2]
) {

  def settings[R0x <: R0, R1x <: R1, R2x <: R2, E1 >: E, Ax >: A](
      managed: ZManaged[R0x with R1x with R2x, E1, Ax]
  ): ExecutableWithoutUsage[R0x with R1x with R2x, E1, Ax] =
    new ExecutableWithoutUsage(managed)

  def settings[R0x <: R0, R1x <: R1, R2x <: R2, E1 >: E, Ax >: A](
      zio: ZIO[R0x with R1x with R2x, E1, Ax]
  ): ExecutableWithoutUsage[R0x with R1x with R2x, E1, Ax] =
    new ExecutableWithoutUsage(ZManaged.fromEffect(zio))
}

class ExecutableWithoutUsage[-R, +E, +A] private[cakeless] (val `env`: ZManaged[R, E, A]) extends AnyVal {
  def runWith[R1 <: R, E1 >: E](usage: A => ZIO[R1, E1, Int]): ExecutableBuilder[R1, E1, A] = use[R1, E1](_.use(usage))

  def runForever: ExecutableBuilder[R, E, A] =
    use[R, E](_.useForever)

  def run(success: A => Int): ExecutableBuilder[R, E, A] = runWith(a => ZIO.succeed(success(a)))

  def handleArgs[R1 <: R, E1 >: E](f: List[String] => ZIO[R1, E1, _]): MainApplicationWithoutUsage[R1, E1, A] =
    new MainApplicationWithoutUsage(`env`, f)

  def ignoreArgs: MainApplicationWithoutUsage[R, E, A] =
    handleArgs[R, E](_ => ZIO.unit)

  private def use[R1 <: R, E1 >: E](f: ZManaged[R1, E1, A] => ZIO[R1, E1, Int]): ExecutableBuilder[R1, E1, A] =
    new ExecutableBuilder[R1, E1, A](`env`, f)
}

class ExecutableBuilder[-R, +E, +A] private[cakeless] (
    val `env`: ZManaged[R @uncheckedVariance, E @uncheckedVariance, A @uncheckedVariance],
    val `use`: ZManaged[R @uncheckedVariance, E @uncheckedVariance, A @uncheckedVariance] => ZIO[R, E, Int]
)

class MainApplicationWithoutUsage[-R, +E, +A] private[cakeless] (
    val `env`: ZManaged[R, E, A],
    `argsHandler`: List[String] => ZIO[R, E, _]
) {

  def runWith[R1 <: R, E1 >: E](usage: A => ZIO[R1, E1, Int]): MainApplicationBuilder[R1, E1, A] = use[R1, E1](_.use(usage))

  def run(success: A => Int): MainApplicationBuilder[R, E, A] = use[R, E](_.use(a => ZIO.succeed(success(a))))

  def runForever: MainApplicationBuilder[R, E, A] =
    use[R, E](_.useForever)

  private def use[R1 <: R, E1 >: E](f: ZManaged[R1, E1, A] => ZIO[R1, E1, Int]): MainApplicationBuilder[R1, E1, A] =
    new MainApplicationBuilder[R1, E1, A](`env`, { (args, managed) =>
      val usedArgs: ZManaged[R, E, _] = ZManaged.fromEffect(`argsHandler`(args))
      val temp                        = usedArgs *> managed
      f(temp)
    })
}

class MainApplicationBuilder[-R, +E, +A] private[cakeless] (
    val `env`: ZManaged[R @uncheckedVariance, E @uncheckedVariance, A @uncheckedVariance],
    val `use`: (List[String], ZManaged[R @uncheckedVariance, E @uncheckedVariance, A @uncheckedVariance]) => ZIO[R, E, Int]
)
