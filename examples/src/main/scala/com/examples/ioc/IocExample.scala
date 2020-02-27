package com.examples.ioc

import cakeless.ioc._
import zio.ZManaged
import zio.ZEnv
import zio.console.putStrLn

object IocExample
    extends ApplicationDefn(
      ofExecutable(Main).ignoreArgs
        .apply {
          putStrLn("Hello, cakeless!")
        }
        .exitM(
          error = e => putStrLn(s"Got error: $e") as -1,
          succeed = _ => putStrLn("Gotbye") as 0
        )
    )

object IocExampleConsole
    extends ApplicationDefn(
      ofExecutable(Main)
        .withArgs { (args, _: ZManaged[ZEnv, Throwable, Any]) =>
          putStrLn(s"Got args: $args") as 0
        }
        //        .aggregate(ConfigurationModuleImpl) todo: implement
        .apply {
          putStrLn("Hello, cakeless!")
        }
        .exitM(
          e => putStrLn(s"Got error: $e") as -1,
          _ => putStrLn("Gotbye") as 0
        )
    )
