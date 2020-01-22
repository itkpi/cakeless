import sbt._
import sbt.Keys.{libraryDependencies, scalaVersion, scalacOptions}

object Dependencies {
  val testV = "3.1.0"
  val shapelessV = "2.3.3"
  val typesafeConfigV = "1.3.3"
  val mockitoV = "2.23.4"
  val macroUtilV = "2.0"
  val zioV = "1.0.0-RC17"
  val simulacrumV = "1.0.0"
  val supertaggedVersion = "1.5"

  object Shapeless {
    val value = "com.chuusai" %% "shapeless" % shapelessV withSources()
  }

  object Zio {
    val zio = "dev.zio" %% "zio" % zioV withSources()
  }

  object Testing {
    val scalatest = "org.scalatest" %% "scalatest" % testV % "test" withSources()
    val scalactic = "org.scalactic" %% "scalactic" % testV withSources()
    val mockito = "org.mockito" % "mockito-core" % mockitoV % "test" withSources()
  }

  object Config {
    val typesafe = "com.typesafe" % "config" % typesafeConfigV withSources()
  }

  object Tagging {
    val supertagged = "org.rudogma" %% "supertagged" % supertaggedVersion withSources()
  }

  object Macros {
    val utils = "com.github.japgolly.microlibs" %% "macro-utils" % macroUtilV withSources()
    val resetallattrs = "org.scalamacros" %% "resetallattrs" % "1.0.0"
    val simulacrum = "org.typelevel" %% "simulacrum" % simulacrumV withSources()
  }

  val kindProjector = "org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full

  private def onScalaVersion[U](`on-2-12`: => U,
                                `on-2-13`: => U): Def.Initialize[U] = Def.setting {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) => `on-2-13`
      case _             => `on-2-12`
    }
  }

  implicit class ProjectOps(private val self: Project) extends AnyVal {
    def withMacroAnnotations(): Project = {
      val options = onScalaVersion(
        `on-2-12` = List(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch)) -> Nil,
        `on-2-13` = Nil -> List("-Ymacro-annotations")
      )
      self.settings(
        libraryDependencies ++= options.value._1,
        scalacOptions ++= options.value._2
      )
    }

    def withCommonSettings(): Project = {
      val option = onScalaVersion(
        `on-2-12` = List("-Ypartial-unification"),
        `on-2-13` = Nil
      )
      self.settings(
        scalacOptions ++= option.value ++ List(
          "-feature"
        ),
        addCompilerPlugin(kindProjector)
      )
    }
  }
}
