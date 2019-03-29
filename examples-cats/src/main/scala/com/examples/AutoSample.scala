package com.examples

import java.nio.file.{Path, Paths}
import cakeless._
import cakeless.tagging._
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object AutoSample {
  val configPath: Path @@ config = Paths.get("examples-cats/src/main/resources/application.conf").tagged[config]
  def main(args: Array[String]): Unit = {
    val props: Map[String, String] @@ props = Map("foo" -> "hostname").tagged[props]
    val c1                                  = cake[AllComponents1 with ExecutionContextComponent with FileConfigComponent]
    val c2                                  = cake[AllComponents2 with ExecutionContextComponent with PropsComponent]
    val program = for {
      c1 <- c1
      c2 <- c2
    } yield {
      c1.getConfigFileAsync.flatMap { conf =>
        println(conf)
        c2.getPropAsync("foo")
          .map(_.getOrElse(throw new IllegalArgumentException("expecting `foo` prop defined")))
          .map(conf.getString)
      }
    }
    val result = program.auto
    println(result)
    println(Await.result(result, Duration.Inf))
  }
}
