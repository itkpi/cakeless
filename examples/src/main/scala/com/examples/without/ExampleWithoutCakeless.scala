package com.examples.without

import java.nio.file.Paths
import com.examples.types.{ConfigPath, Password, Props, Username}
import com.examples._
import zio._
import zio.console._
import scala.concurrent.ExecutionContext

object ExampleWithoutCakeless extends App {
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

    val username: Username         = Username("vitaliihonta")
    val password: Password         = Password("password")
    val propsProd: Props           = Props(Map("host" -> "4.4.4.4"))
    val configPathProd: ConfigPath = ConfigPath(Paths.get("./examples/src/main/resources/application.conf"))

    val wired: IO[IllegalArgumentException, String] = url.provide {
      new NestedComponent(username, password)
        with AllComponents2
        with AllComponents1
        with ExecutionContextComponent
        with PropsComponent
        with FileConfigComponent {

        override implicit val ec: ExecutionContext = ExecutionContext.global
        override val props: Props                  = propsProd
        override val configPath: ConfigPath        = configPathProd
      }
    }
    wired
      .catchAll(e => ZIO.succeed(e.getMessage))
      .flatMap(putStrLn) *> ZIO.succeed(0)
  }
}
