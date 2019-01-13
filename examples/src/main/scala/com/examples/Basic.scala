package com.examples

import cats._
import cats.implicits._
import cats.data.WriterT
import cats.effect.{ExitCode, IO, IOApp}
import java.nio.file.{Path, Paths}
import scala.concurrent.{ExecutionContext, Future}
import com.typesafe.config.{Config, ConfigFactory}
import cakeless._
import cakeless.cats.effect._

trait ExecutionContextComponent {
  implicit def ec: ExecutionContext
}

trait FileConfigComponent {
  def configPath: Path
}

trait PropsComponent {
  def props: Map[String, String]
}

case class Wiring(ec: ExecutionContext, configPath: Path, props: Map[String, String])

trait AllComponents1 { self: ExecutionContextComponent with FileConfigComponent =>
  def getConfigFileAsync: Future[Config] = Future {
    ConfigFactory.parseFile(configPath.toFile)
  }
}

trait AllComponents2 { self: ExecutionContextComponent with PropsComponent =>
  def getPropAsync(prop: String): Future[Option[String]] = Future {
    props get prop
  }
}

trait NestedComponent { self: AllComponents2 with ExecutionContextComponent with PropsComponent =>
  def getConfigAsync(config: Config)(key: String): Future[Option[String]] = Future {
    scala.util.Try(config getString key).toOption
  }
}

object Basic extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    val program = for {
      comp1 <- cakeT[IO, AllComponents1 with ExecutionContextComponent with FileConfigComponent].logged(List("Creating Components 1..."))
      comp2 <- cakeT[IO, AllComponents2 with ExecutionContextComponent with PropsComponent].logged(List("Creating Components 2..."))
      comp3 <- cakeT[IO, NestedComponent with AllComponents2 with ExecutionContextComponent with FileConfigComponent with PropsComponent]
        .logged(List("Creating Nested component..."))
    } yield {
      import comp1.ec
      IO.fromFuture(IO {
        for {
          config <- comp1.getConfigFileAsync
          key    <- comp2.getPropAsync("foo.bar")
          value  <- comp3.getConfigAsync(config)(key getOrElse "")
        } yield value
      })
    }

    val result: WriterT[IO, List[String], IO[Option[String]]] =
      program bake Wiring(
        ec = ExecutionContext.global,
        configPath = Paths.get("examples/src/main/resources/application.conf"),
        props = Map("foo.bar" -> "hostname")
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
