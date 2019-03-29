package com.examples

import java.nio.file.{Path, Paths}
import cakeless._
import scalaz.zio._
import scalaz.zio.console._
import cakeless.tagging._
import scala.concurrent.ExecutionContext.Implicits.global

object AutoSample extends App {
  val configPath: Path @@ config = Paths.get("examples-zio/src/main/resources/application.conf").tagged[config]

  def run(args: List[String]): ZIO[AutoSample.Environment, Nothing, Int] = {
    val props: Map[String, String] @@ props = Map("foo" -> "hostname").tagged[props]
    val c1                                  = cakeZ[Any, Throwable, AllComponents1 with ExecutionContextComponent with FileConfigComponent]
    val c2                                  = cakeZ[Any, Throwable, AllComponents2 with ExecutionContextComponent with PropsComponent]
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
    result
      .flatMap(fut => ZIO.fromFuture(_ => fut))
      .flatMap(putStrLn)
      .foldM(
        err = e => putStrLn(e.getMessage) *> ZIO.succeed(-1),
        succ = _ => ZIO.succeed(0)
      )
  }
}
