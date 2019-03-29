import xerial.sbt.Sonatype._
import Dependencies._

lazy val snapshot: Boolean = false
lazy val v: String = {
  val vv = "0.4.0"
  if (!snapshot) vv
  else vv + "-SNAPSHOT"
}

lazy val scalaReflect = Def.setting {
  "org.scala-lang" % "scala-reflect" % scalaVersion.value
}

organization in ThisBuild := "ua.pp.itkpi"

val `scala-2-12` = "2.12.8"

def sonatypeProject(id: String, base: File) =
  Project(id, base)
    .settings(
      name := id,
      isSnapshot := snapshot,
      version := v,
      scalaVersion := `scala-2-12`,
      publishTo := {
        val nexus = "https://oss.sonatype.org/"
        if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
        else Some("releases" at nexus + "service/local/staging/deploy/maven2")
      },
      scalacOptions ++= Seq("-Ypartial-unification", "-feature"),
      resolvers += Resolver.sonatypeRepo("releases"),
      addCompilerPlugin("org.spire-math"  %% "kind-projector" % "0.9.8"),
      addCompilerPlugin("org.scalamacros" % "paradise"        % "2.1.0" cross CrossVersion.full),
      libraryDependencies ++= Seq(
        Testing.scalactic,
        Testing.scalatest,
        Macros.simulacrum
      )
    )

lazy val kernel = sonatypeProject(id = "cakeless-kernel", base = file("./kernel"))
  .settings(
    libraryDependencies ++= {
      Seq(
        Shapeless.value,
        Macros.utils
      )
    }
  )

lazy val cakelessCats = sonatypeProject(id = "cakeless-cats", base = file("./cats"))
  .dependsOn(kernel)
  .settings(
    libraryDependencies ++= {
      Seq(
        Cats.core,
        Cats.effect,
        Macros.utils,
        Testing.mockito
      )
    }
  )

lazy val cakelessZio = sonatypeProject(id = "cakeless-zio", base = file("./zio"))
  .dependsOn(kernel)
  .settings(
    libraryDependencies ++= {
      Seq(
        Scalaz.zio,
        Macros.utils,
        Testing.mockito
      )
    }
  )

lazy val examplesCats = project
  .in(file("./examples-cats"))
  .dependsOn(kernel, cakelessCats)
  .settings(
    name := "examples-cats",
    version := v,
    scalaVersion := `scala-2-12`,
    scalacOptions += "-Ypartial-unification",
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8"),
    libraryDependencies ++= Seq(
      Config.typesafe
    ),
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")
  )

lazy val examplesZio = project
  .in(file("./examples-zio"))
  .dependsOn(kernel, cakelessZio)
  .settings(
    name := "examples-zio",
    version := v,
    scalaVersion := `scala-2-12`,
    scalacOptions += "-Ypartial-unification",
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8"),
    libraryDependencies ++= Seq(
      Config.typesafe
    ),
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")
  )

lazy val root = project
  .in(file("."))
  .dependsOn(examplesCats, examplesZio)
  .aggregate(kernel, cakelessZio, cakelessCats)
  .settings(
    name := "cakeless-root",
    version := v,
    scalaVersion := `scala-2-12`,
    scalacOptions += "-Ypartial-unification",
    isSnapshot := snapshot,
    skip in publish := true,
    publish := {},
    publishLocal := {},
    coverageExcludedPackages := ".*operations.*",
    coverageExcludedFiles := ".*orderingInstances | .*arrows* | .*ToCaseClass*"
  )
