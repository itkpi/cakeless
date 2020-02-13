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

trait ExecutionContextComponentImpl extends ExecutionContextComponent {
  override implicit val ec: ExecutionContext = ExecutionContext.global
}

trait FileConfigComponent {
  def configPath: ConfigPath
}

trait PropsComponent {
  def props: Props
}

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

class Database(url: DbUrl) {
  def openConnection(): ZIO[Console with Random, ConnectionFailureException, Unit] =
    nextBoolean.flatMap {
      case true  => ZIO.fail(new ConnectionFailureException(s"Unable to open connection to DB $url ..."))
      case false => putStrLn(s"Opened connection with DB $url")
    }

  def runSql(sql: String): Task[String] = Task {
    println(s"[DB $url] EXECUTING: $sql")
    "1"
  }

  def close(): URIO[Console, Unit] = putStrLn(s"Closed connenction with DB $url!")
}

trait DbComponent {
  def db: Database
}
