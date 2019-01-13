import sbt._

object Dependencies {
  val testV           = "3.0.4"
  val shapelessV      = "2.3.3"
  val catsV           = "1.5.0"
  val catsEffectV     = "1.1.0"
  val typesafeConfigV = "1.3.3"

  object Shapeless {
    val value = "com.chuusai" %% "shapeless" % shapelessV withSources ()
  }

  object Cats {
    val core   = "org.typelevel" %% "cats-core"   % catsV withSources ()
    val effect = "org.typelevel" %% "cats-effect" % catsEffectV
  }

  object Testing {
    val scalatest = "org.scalatest" %% "scalatest" % testV % "test" withSources ()
    val scalactic = "org.scalactic" %% "scalactic" % testV withSources ()
  }

  object Config {
    val typesafe = "com.typesafe" % "config" % typesafeConfigV withSources ()
  }
}