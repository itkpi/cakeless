package com.examples

import java.nio.file.Paths
import cakeless._
import cakeless.lifecycle._
import cakeless.cats_effect._
import cats.effect.{ExitCode, IO, IOApp}
import cats._
import cats.implicits._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import cakeless.tagging._
import shapeless.syntax.singleton._

object LifecycleExample extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    val comp1 = cake[AllComponents1 with ExecutionContextComponent with FileConfigComponent]
      .delayed[IO]
      .preStart {
        println("Components 1 preStart.. ")
      }
      .postStart {
        println("Components 1 started!")
      }

    val comp2 = cake[AllComponents2 with ExecutionContextComponent with PropsComponent with DbComponent]
      .delayed[IO]
      .singleton // ensures that component will be created exactly once
      .postStartUseF { comp =>
        println("Opening connection with DB...")
        comp.db.openConnection().recoverWith {
          case e: ConnectionFailureException =>
            println(e)
            0.tailRecM[IO, Unit] {
              case 10 =>
                println("Max attempts reached...")
                IO.raiseError(e)

              case i =>
                println(s"Open connection: attempt $i")
                IO.sleep(i.seconds) *> comp.db
                  .openConnection()
                  .redeem(
                    recover = { case e: ConnectionFailureException => (i + 1).asLeft[Unit] },
                    map = _.asRight[Int]
                  )
            }
        }
      }
      .recoverWith {
        case e: ConnectionFailureException =>
          println("Failed to open connection with DB..")
          IO.raiseError(e)
      }

    val program = comp1.flatMap { comp1 =>
      comp2.bracket(use = { comp2 =>
        IO(println("Your program here!"))
      })(release = _.db.close())
    }

    program.as[WiringWithDb] bake WiringWithDb(
      ec = ExecutionContext.global,
      configPath = Paths.get("/") taggedAs "config",
      props = Map("foo" -> "bar") taggedAs "props",
      db = new Database[IO]
    ) redeem (
      recover = _ => ExitCode.Error,
      map = _ => ExitCode.Success
    )
  }
}
