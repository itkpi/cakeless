package com.examples.ioc

import java.nio.file.Paths

import cakeless._
import cakeless.ioc._
import cakeless.compiletime._
import com.examples.types.{ConfigPath, Props}
import com.examples.{FileConfigComponent, PropsComponent}
import zio._

object ConfigurationModule extends UModuleDecl[FileConfigComponent with PropsComponent]

object ConfigurationModuleImpl
    extends ModuleDefn(
      of(ConfigurationModule) {
        val configPathImpl: ConfigPath = ConfigPath(Paths.get("./examples/src/main/resources/application.conf"))
        val propsImpl: Props           = Props(Map("host" -> "localhost"))

        ZIO.environment[FileConfigComponent with PropsComponent].inject0.wire
      }
    )
