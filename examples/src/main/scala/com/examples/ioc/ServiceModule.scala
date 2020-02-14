package com.examples.ioc

import cakeless._
import cakeless.ioc._
import cakeless.compiletime._
import com.examples.types.{Props, Token}
import com.examples.{AllComponents2, DbComponent, ExecutionContextComponent, NestedComponent, PropsComponent}
import zio._
import scala.concurrent.ExecutionContext

object ServiceModule extends UModuleDecl[NestedComponent with DbComponent]

object ServiceModuleImpl
    extends ModuleDefn(
      of(ServiceModule)
        .dependsOn(
          ExecutionModule,
          ConfigurationModule,
          DatabaseModule
        )
        .settings(
          ZIO.accessM[ExecutionContextComponent with PropsComponent] { parent =>
            import parent._
//            val propsImpl             = parent.props
            implicit val token: Token = Token("safjginkl352")
            ZIO.environment[NestedComponent with AllComponents2].inject0.wire
          }
        )
    )
