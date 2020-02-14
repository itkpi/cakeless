package com.examples.base

import java.nio.file.Paths
import cakeless._
import cakeless.nat._
import cakeless.compiletime._
import com.examples.types._
import com.examples.{AllComponents1, AllComponents2, NestedComponent}
import zio._
import zio.console._
import scala.concurrent.ExecutionContext.Implicits.global

object Example extends App {
  val propsProd: Props        = Props(Map("host" -> "4.4.4.4"))
  @wired val propsTest: Props = Props(Map("host" -> "localhost")) // todo: you may annotate propsProd instead any see what happens :)

  val configPathProd: ConfigPath = ConfigPath(Paths.get("./examples/src/main/resources/application.conf"))
  implicit val token: Token      = Token("safjginkl352")

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {

    val configValue =
      ZIO
        .accessM[AllComponents1] { c =>
          c.getConfigFile
        }
        .mapError(e => new IllegalArgumentException("Config missing", e))

    val component2 = ZIO.environment[NestedComponent with AllComponents2]

    val hostValue = component2.flatMap { c =>
      c.getProp("host").flatMap {
        case Some(host) => ZIO.succeed(host)
        case None       => ZIO.fail(new IllegalArgumentException("host not found"))
      }
    }

    val url: ZIO[NestedComponent with AllComponents2 with AllComponents1, IllegalArgumentException, String] = for {
      config <- configValue
      c2     <- component2
      port   <- c2.getConfig(config)("http.port").map(_.getOrElse(80))
      host   <- hostValue
    } yield s"$host:$port?token=${c2.token}"

//     todo: uncomment these 3 lines and comment `injectPrimary` to see how specific constructor selection works like
//    val username: Username                          = Username("vitaliihonta")
//    val password: Password                          = Password("password")
//    val wired: IO[IllegalArgumentException, String] = url.inject[_1].wire

    val wired: IO[IllegalArgumentException, String] = url.inject0.wire
    wired
      .catchAll(e => ZIO.succeed(e.getMessage))
      .flatMap(putStrLn) *> ZIO.succeed(0)
  }
}
