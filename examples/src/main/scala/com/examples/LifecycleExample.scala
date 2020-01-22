package com.examples

import java.nio.file.Paths
import cakeless._
import cakeless.internal.InjectionMagnet
import com.examples.types.{ConfigPath, Props}
import com.typesafe.config.{Config, ConfigException}
import zio._
import zio.console._
import zio.random._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

object LifecycleExample extends App {
  val configPath: ConfigPath = ConfigPath(Paths.get("./examples/src/main/resources/application.conf"))
  val props: Props           = Props(Map("host" -> "localhost"))
  val db: Database           = new Database

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    val comp1: ZIO[Console, ConfigException, Config] = injectPrimary(
      ZIO.accessM[AllComponents1](_.getConfigFile).tap(c => putStrLn(c.toString)),
      Lifecycle
        .preStart(
          putStrLn("Component 1 preStart")
        )
        .postStart(
          putStrLn("Component 1 started!")
        )
    )

    val program: ZIO[Console with Random, ConfigException, String] = comp1.flatMap { config =>
      val table = config.getString("table")
      val getFromDb: URIO[AllComponents2 with DbComponent, String] = ZIO.accessM[AllComponents2 with DbComponent] { c2 =>
        c2.db
          .runSql(s"SELECT * FROM $table")
          .catchAll(e => ZIO.succeed(e.getMessage))
      }

      val x: Lifecycle[Any, Console with Random, DbComponent] = Lifecycle.postStart { (comp2: DbComponent) =>
        val open = putStrLn("Opening connection with DB...") *> comp2.db.openConnection()

        open.retry(Schedule.recurs(10)).orDie
      }

      injectPrimary(getFromDb, x)
    }

    program.fold(_ => 1, _ => 0)
  }
}
