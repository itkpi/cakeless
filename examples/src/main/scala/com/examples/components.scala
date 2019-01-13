package com.examples

import java.nio.file.Path

import cats.effect.IO
import cats.{Monad, MonadError}
import com.typesafe.config.{Config, ConfigFactory}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds

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

case class WiringWithDb(ec: ExecutionContext, configPath: Path, props: Map[String, String], db: Database[IO])

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

class ConnectionFailureException(msg: String) extends Exception(msg)

class Database[F[_]](implicit F: MonadError[F, Throwable]) {
  def openConnection(): F[Unit] =
    if (scala.util.Random.nextInt(10) <= 7) F.raiseError(new ConnectionFailureException("Unable to open connection to DB..."))
    else F.pure(println("Opened connection to database!"))

  def close(): F[Unit] = F.pure(println("Closed connenction with DB!"))
}

trait DbComponent {
  def db: Database[IO]
}
