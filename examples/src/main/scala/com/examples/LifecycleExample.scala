package com.examples

import java.nio.file.Paths
import cakeless._
import com.examples.types.{ConfigPath, Props}
import com.typesafe.config.{Config, ConfigException}
import zio._
import zio.console._
import zio.random._
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global

object LifecycleExample extends App {
  val configPathImpl: ConfigPath = ConfigPath(Paths.get("./examples/src/main/resources/application.conf"))
  val propsImpl: Props           = Props(Map("host" -> "localhost"))
  val dbImpl: Database           = new Database

  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] = {
    val comp1: ZIO[Console, ConfigException, Config] =
      ZIO
        .accessM[AllComponents1](_.getConfigFile)
        .tap(c => putStrLn(c.toString))
        .injectPrimary
        .withLifecycle(
          Lifecycle.preStart(
            putStrLn("Component 1 preStart")
          ) && Lifecycle.postStart(
            putStrLn("Component 1 started!")
          )
        )
        .excludeZEnv[Console]
        .wire

    val program: ZIO[Console with Random, ConfigException, String] = comp1.flatMap { config =>
      val table = config.getString("table")
      val getFromDb: URIO[AllComponents2 with DbComponent, String] = ZIO.accessM[AllComponents2 with DbComponent] { c2 =>
        c2.db
          .runSql(s"SELECT * FROM $table")
          .catchAll(e => ZIO.succeed(e.getMessage))
      }

      getFromDb.injectPrimary
        .withLifecycle {
          Lifecycle.postStart { comp2: DbComponent =>
            val open = putStrLn("Opening connection with DB...") *> comp2.db.openConnection()

            open.retry(Schedule.recurs(10)).orDie
          }
        }
        .excludeZEnv[Console with Random]
        .wire
    }

    program.fold(_ => 1, _ => 0)
  }
}
