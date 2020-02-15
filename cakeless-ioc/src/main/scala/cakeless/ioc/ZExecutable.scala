package cakeless.ioc

import zio._

sealed trait ZExecutable[-Rx, +Ex, +Ax] extends ZModuleDecl[Rx, Ex, Ax] {}

trait ZExecutableDecl[-Rx, +Ex, +Ax] extends ZExecutable[Rx, Ex, Ax]

trait ExecutableDecl[+Ex, +Ax]    extends ZExecutableDecl[Any, Ex, Ax]
trait RExecutableDecl[-Rx, +Ax]   extends ZExecutableDecl[Rx, Throwable, Ax]
trait UExecutableDecl[+Ax]        extends ZExecutableDecl[Any, Nothing, Ax]
trait URExecutableeDecl[-Rx, +Ax] extends ZExecutableDecl[Rx, Nothing, Ax]

class ExecutableDefn[-Rx, +Ex, +Ax](builder: ModuleBuilder[Rx, Ex, Ax]) extends ZExecutableDecl[Rx, Ex, Ax]

class ApplicationDefn[+Ax](builder: ModuleBuilder[ZEnv, Nothing, Ax]) extends ZExecutableDecl[ZEnv, Nothing, Ax] with App {
  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    builder.`env`.use(ZIO.succeed).as(0)
}
