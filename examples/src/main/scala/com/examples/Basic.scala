package com.examples
import java.nio.file.{Path, Paths}

import cats._

import scala.concurrent.ExecutionContext
import com.github.cakeless._
import com.github.cakeless.internal.UnUnion
import shapeless.{::, HNil}

object Basic {
  def main(args: Array[String]): Unit = {
    val res = for {
      comp  <- cake[Component1]
      comp2 <- cake[Component2]
    } yield comp

    res.bake(ExecutionContext.global :: Paths.get("path") :: Seq("prop1", "prop2") :: HNil)

    res bake Wiring(ExecutionContext.global, configPath = Paths.get("path"), props = Seq("prop1", "prop2"))
  }

  case class Wiring(ec: ExecutionContext, configPath: Path, props: Seq[String])

  type Component1 = ExecutionContextComponent with FileConfigComponent
  type Component2 = ExecutionContextComponent with PropsComponent

  trait ExecutionContextComponent {
    implicit def ec: ExecutionContext
  }

  trait FileConfigComponent {
    def configPath: Path
  }

  trait PropsComponent {
    def props: Seq[String]
  }
}
