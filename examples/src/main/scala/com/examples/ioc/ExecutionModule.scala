package com.examples.ioc

import cakeless.module._
import com.examples.{ExecutionContextComponent, ExecutionContextComponentImpl}
import zio.ZIO

object ExecutionModule extends UModuleDecl[ExecutionContextComponent]

object ExecutionModuleImpl
    extends ModuleDefn(
      of(ExecutionModule) {
        ZIO.succeed(new ExecutionContextComponentImpl {})
      }
    )
