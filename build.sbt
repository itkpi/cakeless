import Dependencies._

lazy val scalaReflect = Def.setting {
  "org.scala-lang" % "scala-reflect" % scalaVersion.value
}

organization in ThisBuild := "ua.pp.itkpi"

val `scala-2-12` = "2.12.10"
val `scala-2-13` = "2.13.1"

scalaVersion in ThisBuild := `scala-2-13`
crossScalaVersions in ThisBuild := Seq(`scala-2-12`, `scala-2-13`)

def sonatypeProject(id: String, base: File) =
  Project(id, base)
    .settings(
      name := id,
      publishTo := {
        val nexus = "https://oss.sonatype.org/"
        if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
        else Some("releases" at nexus + "service/local/staging/deploy/maven2")
      },
      resolvers += Resolver.sonatypeRepo("releases"),
      libraryDependencies ++= Seq(
        Testing.scalactic,
        Testing.scalatest,
        Macros.simulacrum,
        Macros.resetallattrs
      )
    )
    .withMacroAnnotations()
    .withCommonSettings()

lazy val `cakeless-new` = sonatypeProject(id = "cakeless-new", base = file("./cakeless"))
  .settings(
    libraryDependencies ++= {
      Seq(
        Shapeless.value,
        Macros.utils,
        Zio.zio,
        Testing.mockito
      )
    }
  )

lazy val examples = project
  .in(file("./examples"))
  .dependsOn(`cakeless-new`)
  .settings(
    name := "examples",
    libraryDependencies ++= Seq(
      Config.typesafe,
      Tagging.supertagged
    )
  )
  .withCommonSettings()

lazy val root = project.in(file("."))
  .aggregate(`cakeless-new`, examples)
  .settings(
    name := "cakeless-new-root",
    skip in publish := true,
    publish := {},
    publishLocal := {},
    coverageExcludedPackages := ".*operations.*",
    coverageExcludedFiles := ".*orderingInstances | .*arrows* | .*ToCaseClass*"
  )
  .withCommonSettings()