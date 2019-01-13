package com.examples

import cats._
import cats.syntax.all._
import cats.effect.{ExitCode, IO, IOApp}
import java.nio.file.{Path, Paths}
import scala.concurrent.{ExecutionContext, Future}
import com.typesafe.config.{Config, ConfigFactory}
import cakeless._

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
      comp1 <- cake[AllComponents1 with ExecutionContextComponent with FileConfigComponent]
      comp2 <- cake[AllComponents2 with ExecutionContextComponent with PropsComponent]
      comp3 <- cake[NestedComponent with AllComponents2 with ExecutionContextComponent with FileConfigComponent with PropsComponent]
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

    val resultIO = program bake Wiring(
      ec = ExecutionContext.global,
      configPath = Paths.get("examples/src/main/resources/application.conf"),
      props = Map("foo.bar" -> "hostname")
    )

    resultIO.map(println).as(ExitCode.Success)
  }

}
