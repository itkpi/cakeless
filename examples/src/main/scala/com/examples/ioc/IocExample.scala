package com.examples.ioc

import cakeless.ioc._
import zio.console.putStrLn

object IocExample
    extends ApplicationDefn(
      ofExecutable(Main) {
        putStrLn("Hello, cakeless!")
      }.ignoreArgs.run(_ => 0)
    )
