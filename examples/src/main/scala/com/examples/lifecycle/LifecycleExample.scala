//package com.examples.lifecycle
//
//import java.nio.file.Paths
//
//import cakeless._
//import cakeless.ioc._
//import cakeless.compiletime._
//import com.examples.types.{ConfigPath, DbUrl, Props}
//import com.examples.{AllComponents1, Database, DbComponent}
//import com.typesafe.config.{Config, ConfigException}
//import zio._
//import zio.console._
//import zio.random._
//
//object LifecycleExample extends App {
//  val configPathImpl: ConfigPath = ConfigPath(Paths.get("./examples/src/main/resources/application.conf"))
//  val propsImpl: Props           = Props(Map("host" -> "localhost"))
//  val dbUrl: DbUrl               = DbUrl("jdbc:postgresql://localhost:5432/my_db")
//
//  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
//    val comp1: ZIO[Console, ConfigException, Config] =
//      ZIO
//        .accessM[AllComponents1](_.getConfigFile)
//        .tap(c => putStrLn(c.toString))
//        .inject0
//        .withLifecycle(
//          Lifecycle.preStart(
//            putStrLn("Component 1 preStart")
//          ) && Lifecycle.postStart(
//            putStrLn("Component 1 started!")
//          )
//        )
//        .excludeZEnv[Console]
//        .wire
//
//    val databaseManaged: URManaged[Console, Database] = ZManaged
//      .make[Console, Nothing, Database](
//        ZIO.effectTotal(new Database(dbUrl))
//      )(release = _.close())
//
//    val selectionIO = comp1.flatMap { config =>
//      val table = config.getString("table")
//      ZIO.accessM[DbComponent with Console] { c2 =>
//        c2.db
//          .runSql(s"SELECT * FROM $table")
//          .catchAll(e => ZIO.succeed(e.getMessage)) >>= putStrLn
//      }
//    }
//
//    val program: ZManaged[Console with Random, ConfigException, Unit] = databaseManaged.flatMap { database =>
//      ZManaged
//        .fromEffect(selectionIO)
//        .inject0
//        .withLifecycle {
//          Lifecycle.postStart { comp2: DbComponent =>
//            val open = putStrLn("Opening connection with DB...") *> comp2.db.openConnection()
//
//            open.retry(Schedule.recurs(10)).orDie
//          }
//        }
//        .excludeZEnv[Console with Random]
//        .wire
//    }
//
//    program.use(_ => putStrLn("DONE")).fold(_ => 1, _ => 0)
//  }
//}
