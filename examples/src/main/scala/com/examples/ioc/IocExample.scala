package com.examples.ioc

import cakeless.ioc._
import zio.console.putStrLn

object IocExample
    extends ApplicationDefn(
      of(Main) {
        putStrLn("Hello, cakeless!")
      }
    )
