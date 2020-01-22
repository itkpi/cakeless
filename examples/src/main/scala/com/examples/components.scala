package com.examples

import zio._
import zio.console._
import zio.random._
import com.typesafe.config.{Config, ConfigException, ConfigFactory}
import scala.concurrent.{ExecutionContext, Future}
import types._

trait ExecutionContextComponent {
  implicit def ec: ExecutionContext
}

trait FileConfigComponent {
  def configPath: ConfigPath
}

trait PropsComponent {
  def props: Props
}

case class Wiring(ec: ExecutionContext, configPath: ConfigPath, props: Props, token: Token)

case class WiringWithDb(ec: ExecutionContext, configPath: ConfigPath, props: Props, db: Database)

trait AllComponents1 {
  self: FileConfigComponent =>

  def getConfigFile: IO[ConfigException, Config] =
    IO.effect {
        ConfigFactory.parseFile(configPath.toFile)
      }
      .refineToOrDie[ConfigException]
}

trait AllComponents2 { self: ExecutionContextComponent with PropsComponent =>
  def getProp(prop: String): UIO[Option[String]] = {
    def legacyFutureCode = Future {
      props get prop
    }

    ZIO.fromFuture { _ =>
      legacyFutureCode
    }.orDie
  }
}

class NestedComponent(implicit val token: Token) {
  self: AllComponents2 with ExecutionContextComponent with PropsComponent =>

  def this(username: Username, password: Password) =
    this()(Token(s"$username@$password"))

  def getConfig(config: Config)(key: String): UIO[Option[String]] =
    IO.effect {
      config getString key
    }.option
}

class ConnectionFailureException(msg: String) extends Exception(msg)

class Database {
  def openConnection(): ZIO[Console with Random, ConnectionFailureException, Unit] =
    nextBoolean.flatMap {
      case true  => ZIO.fail(new ConnectionFailureException("Unable to open connection to DB..."))
      case false => putStrLn("Opened connection with DB")
    }

  def runSql(sql: String): Task[String] = Task {
    println(s"EXECUTING: $sql")
    "1"
  }

  def close(): URIO[Console, Unit] = putStrLn("Closed connenction with DB!")
}

trait DbComponent {
  def db: Database
}
