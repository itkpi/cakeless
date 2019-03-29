package com.examples

import cats.implicits._
import cakeless._
import cats.data.WriterT
import cats.effect.{ExitCode, IO, IOApp}
import java.nio.file.{Path, Paths}
import scala.concurrent.ExecutionContext
import cakeless.tagging._

object Basic extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    val program = for {
      comp1 <- {
        cakeDelayed[IO, AllComponents1 with ExecutionContextComponent with FileConfigComponent]
          .logged(List("Creating Components 1..."))
      }

      comp2 <- {
        cakeDelayed[IO, AllComponents2 with ExecutionContextComponent with PropsComponent]
          .logged(List("Creating Components 2..."))
      }

      comp3 <- {
        cakeDelayed[IO, NestedComponent with AllComponents2 with ExecutionContextComponent with FileConfigComponent with PropsComponent]
          .logged(List("Creating Nested component..."))
      }

    } yield {
      import comp1.ec
      IO.fromFuture(IO {
        for {
          config <- comp1.getConfigFileAsync
          key    <- comp2.getPropAsync("foo.bar")
          value  <- comp3.getConfigAsync(config)(key getOrElse "")
        } yield value.map(_ + s"/${comp3.token}")
      })
    }

    val result: WriterT[IO, List[String], IO[Option[String]]] =
      program.as[Wiring] bake Wiring(
        ec = ExecutionContext.global,
        configPath = Paths.get("examples/src/main/resources/application.conf").tagged[config],
        props = Map("foo.bar" -> "hostname").tagged[props],
        token = "1232asd".tagged[token]
      ) tell List("Created cake!")

    result.run
      .flatMap {
        case (log, value) =>
          value.map { value =>
            log.foreach(println)
            println(s"Result: $value")
          }
      }
      .as(ExitCode.Success)
  }

}