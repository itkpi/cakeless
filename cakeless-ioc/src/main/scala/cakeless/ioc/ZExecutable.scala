package cakeless.ioc

import cakeless.ioc.builder.{ZExecutableBuilder, ZMainBuilder, ZUsage}
import zio._

sealed trait ZExecutable[-Rx, +Ex, +Ax] extends ZModuleDecl[Rx, Ex, Ax] {}

trait ZExecutableDecl[-Rx, +Ex, +Ax] extends ZExecutable[Rx, Ex, Ax]

trait ExecutableDecl[+Ex, +Ax]    extends ZExecutableDecl[Any, Ex, Ax]
trait RExecutableDecl[-Rx, +Ax]   extends ZExecutableDecl[Rx, Throwable, Ax]
trait UExecutableDecl[+Ax]        extends ZExecutableDecl[Any, Nothing, Ax]
trait URExecutableeDecl[-Rx, +Ax] extends ZExecutableDecl[Rx, Nothing, Ax]

sealed trait ZExecutableAsManaged[-Rx, +Ex, +Ax] extends ZExecutableDecl[Rx, Ex, Ax] {
  def asManaged: ZManaged[Rx, Ex, Ax]
}

class ExecutableDefn[-Rx, +Ex, +Ax](builder: ZExecutableBuilder[Rx, Ex, Ax, ZUsage.NoUsage]) extends ZExecutableAsManaged[Rx, Ex, Ax] {
  final override def asManaged: ZManaged[Rx, Ex, Ax] = builder.getManaged

  def execute: ZIO[Rx, Ex, Ax] = builder.getUsage[Rx, Ex, Ax].use(Nil)(asManaged)
}

class ApplicationDefn[+Ex, +Ax](builder: ZMainBuilder[Ex, Ax, ZUsage.Base]) extends ZExecutableAsManaged[ZEnv, Ex, Ax] with App {

  final override def asManaged: ZManaged[zio.ZEnv, Ex, Ax] = builder.getManaged

  final override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    builder.getUsage
      .use(args)(asManaged)
      .foldM(
        builder.failure,
        builder.success
      )
}
