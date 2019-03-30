package com.examples

import java.nio.file.Paths
import cakeless._
import cakeless.tagging._
import scalaz.zio._
import scalaz.zio.console._
import scala.concurrent.ExecutionContext

object Basic extends App {
  def run(args: List[String]): ZIO[Basic.Environment, Nothing, Int] = {
    val program = for {
      comp1 <- {
        cakeZ[AllComponents1 with ExecutionContextComponent with FileConfigComponent]
          .logged("Creating Components 1...")
      }

      comp2 <- {
        cakeZ[AllComponents2 with ExecutionContextComponent with PropsComponent]
          .logged("Creating Components 2...")
      }

      comp3 <- {
        cakeZ[NestedComponent with AllComponents2 with ExecutionContextComponent with FileConfigComponent with PropsComponent]
          .logged("Creating Nested component...")
      }

    } yield {
      ZIO.fromFuture { implicit ec =>
        for {
          config <- comp1.getConfigFileAsync
          key    <- comp2.getPropAsync("foo.bar")
          value  <- comp3.getConfigAsync(config)(key getOrElse "")
        } yield value.map(_ + s"/${comp3.token}")
      }
    }

    val result: ZIO[Console, Throwable, Option[String]] =
      program
        .as[Wiring]
        .bake(
          Wiring(
            ec = ExecutionContext.global,
            configPath = Paths.get("examples/src/main/resources/application.conf").tagged[config],
            props = Map("foo.bar" -> "hostname").tagged[props],
            token = "1232asd".tagged[token]
          )
        )
        .flatten <* putStrLn("Created cake!")

    result
      .flatMap { value =>
        putStrLn(s"Result: $value")
      }
      .fold(
        err = _ => -1,
        succ = _ => 0
      )
  }
}
