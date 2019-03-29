package com.examples

import java.nio.file.Path

import scalaz.zio._
import scalaz.zio.console._
import com.typesafe.config.{Config, ConfigFactory}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds
import cakeless.tagging._

import scala.util.Random

trait ExecutionContextComponent {
  implicit def ec: ExecutionContext
}

trait FileConfigComponent {
  def configPath: Path @@ config
}

trait PropsComponent {
  def props: Map[String, String] @@ props
}

case class Wiring(ec: ExecutionContext, configPath: Path @@ config, props: Map[String, String] @@ props, token: String @@ token)

case class WiringWithDb(ec: ExecutionContext, configPath: Path @@ config, props: Map[String, String] @@ props, db: Database)

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

class NestedComponent(implicit val token: String @@ token) { self: AllComponents2 with ExecutionContextComponent with PropsComponent =>
  def getConfigAsync(config: Config)(key: String): Future[Option[String]] = Future {
    scala.util.Try(config getString key).toOption
  }
}

class ConnectionFailureException(msg: String) extends Exception(msg)

class Database {
  def openConnection(): ZIO[Console, Throwable, Unit] =
    if (Random.nextBoolean()) ZIO.fail(new ConnectionFailureException("Unable to open connection to DB..."))
    else putStrLn("Opened connection with DB")

  def close(): ZIO[Console, Nothing, Unit] = putStrLn("Closed connenction with DB!")
}

trait DbComponent {
  def db: Database
}
