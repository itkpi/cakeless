import xerial.sbt.Sonatype._
import Dependencies._

lazy val snapshot: Boolean = true
lazy val v: String = {
  val vv = "0.3.0"
  if (!snapshot) vv
  else vv + "-SNAPSHOT"
}

lazy val scalaReflect = Def.setting {
  "org.scala-lang" % "scala-reflect" % scalaVersion.value
}

organization in ThisBuild := "ua.pp.itkpi"

val `scala-2-12` = "2.12.8"
val `scala-2-11` = "2.11.12"

def sonatypeProject(id: String, base: File) =
  Project(id, base)
    .settings(
      name := id,
      isSnapshot := snapshot,
      version := v,
      scalaVersion := `scala-2-12`,
      crossScalaVersions := Seq(`scala-2-11`, `scala-2-12`),
      publishTo := {
        val nexus = "https://oss.sonatype.org/"
        if (isSnapshot.value)
          Some("snapshots" at nexus + "content/repositories/snapshots")
        else
          Some("releases" at nexus + "service/local/staging/deploy/maven2")
      },
      scalacOptions ++= Seq("-Ypartial-unification", "-feature"),
      resolvers += Resolver.sonatypeRepo("releases"),
      addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8"),
      libraryDependencies ++= Seq(
        Testing.scalactic,
        Testing.scalatest
      )
    )

lazy val kernel = sonatypeProject(id = "cakeless", base = file("./kernel"))
  .settings(
    libraryDependencies ++= {
      Seq(
        Shapeless.value,
        Cats.core,
        Macros.utils,
        Testing.mockito,
        Cats.effect
      )
    }
  )

lazy val examples = project
  .in(file("./examples"))
  .dependsOn(kernel)
  .settings(
    name := "examples",
    version := v,
    scalaVersion := `scala-2-12`,
    crossScalaVersions := Seq(`scala-2-11`, `scala-2-12`),
    scalacOptions += "-Ypartial-unification",
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8"),
    libraryDependencies ++= Seq(
      Config.typesafe
    ),
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")
  )

lazy val root = project
  .in(file("."))
  .dependsOn(examples)
  .aggregate(kernel)
  .settings(
    name := "cakeless-root",
    version := v,
    scalaVersion := `scala-2-12`,
    crossScalaVersions := Seq(`scala-2-11`, `scala-2-12`),
    scalacOptions += "-Ypartial-unification",
    isSnapshot := snapshot,
    skip in publish := true,
    publish := {},
    publishLocal := {},
    coverageExcludedPackages := ".*operations.*",
    coverageExcludedFiles := ".*orderingInstances | .*arrows* | .*ToCaseClass*"
  )
