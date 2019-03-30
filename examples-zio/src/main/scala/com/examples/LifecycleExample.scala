package com.examples

import java.nio.file.Paths

import cakeless._
import scalaz.zio._
import scalaz.zio.console._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import cakeless.tagging._
import shapeless.syntax.singleton._

object LifecycleExample extends App {
  def run(args: List[String]): ZIO[LifecycleExample.Environment, Nothing, Int] = {
    val comp1 = cakeZ[AllComponents1 with ExecutionContextComponent with FileConfigComponent]
      .preStart {
        println("Components 1 preStart.. ")
      }
      .postStart {
        println("Components 1 started!")
      }

    val comp2 =
      cakeSingleton[AllComponents2 with ExecutionContextComponent with PropsComponent with DbComponent]
        .postStartUseF { comp =>
          val open = putStrLn("Opening connection with DB...") *> comp.db.openConnection()

          open.retry(ZSchedule.recurs(10))
        }
        .catchSomeWith {
          case e: ConnectionFailureException =>
            println("Failed to open connection with DB..")
            IO.fail(e)
        }

    val program = comp1.flatMap { comp1 =>
      comp2.bracket(release = _.db.close())(use = { comp2 =>
        putStrLn("Your program here!")
      })
    }

    program
      .as[WiringWithDb]
      .bake(
        WiringWithDb(
          ec = ExecutionContext.global,
          configPath = Paths.get("/") taggedAs "config",
          props = Map("foo" -> "bar") taggedAs "props",
          db = new Database
        )
      )
      .fold(
        err = _ => -1,
        succ = _ => 0
      )
  }
}
