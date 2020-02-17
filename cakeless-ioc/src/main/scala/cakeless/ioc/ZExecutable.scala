package cakeless.ioc

import cakeless.ioc.builder.{ExecutableBuilder, MainApplicationBuilder}
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

class ExecutableDefn[-Rx, +Ex, +Ax](builder: ExecutableBuilder[Rx, Ex, Ax]) extends ZExecutableAsManaged[Rx, Ex, Ax] {
  final override def asManaged: ZManaged[Rx, Ex, Ax] = builder.`env`

  def execute: ZIO[Rx, Ex, Int] = builder.`use`(asManaged)
}

class ApplicationDefn[+Ax](builder: MainApplicationBuilder[ZEnv, Nothing, Ax]) extends ZExecutableAsManaged[ZEnv, Nothing, Ax] with App {

  final override def asManaged: ZManaged[zio.ZEnv, Nothing, Ax] = builder.`env`

  final override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    builder.`use`(args, asManaged)
}
