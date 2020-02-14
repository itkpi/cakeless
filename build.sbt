import BuildConfig._

organization in ThisBuild := "ua.pp.itkpi"

val `scala-2-12` = "2.12.10"
val `scala-2-13` = "2.13.1"

scalaVersion in ThisBuild := `scala-2-13`
crossScalaVersions in ThisBuild := Seq(`scala-2-12`, `scala-2-13`)

def sonatypeProject(id: String, base: File) =
  Project(id, base)
    .settings(
      name := id,
      resolvers += Resolver.sonatypeRepo("releases"),
      libraryDependencies ++= Seq(
        )
    )
    .withMacro()
    .withMacroAnnotations()
    .withCommonSettings()

lazy val `cakeless-injector` = sonatypeProject(id = "cakekess-injector", base = file("./cakeless-injector"))
  .settings(
    libraryDependencies ++= Seq(
      )
  )

lazy val `cakeless-compiletime` = sonatypeProject(id = "cakeless-compiletime", base = file("./cakeless-compiletime"))
  .dependsOn(`cakeless-injector`)
  .settings(
    libraryDependencies ++= Seq(
      Macros.utils,
      Macros.resetallattrs
    )
  )

lazy val `cakeless-ioc` = sonatypeProject(id = "cakeless-ioc", base = file("./cakeless-ioc"))
  .settings(
    libraryDependencies ++= {
      Seq(
        Zio.zio
      )
    }
  )
  .withZioTest()

lazy val cakeless = sonatypeProject(id = "cakeless", base = file("./cakeless"))
  .dependsOn(
    `cakeless-injector`,
    `cakeless-compiletime`,
    `cakeless-ioc`
  )
  .withZioTest()

lazy val examples = project
  .in(file("./examples"))
  .dependsOn(cakeless)
  .settings(
    skip in publish := true,
    publish := {},
    publishLocal := {},
    name := "examples",
    libraryDependencies ++= Seq(
      Config.typesafe,
      Tagging.supertagged
    ),
    coverageEnabled := false
  )
  .withCommonSettings()

lazy val docs = project
  .in(file("cakeless-docs"))
  .settings(
    mdocVariables := Map(
      "VERSION" -> version.value
    ),
    coverageEnabled := false,
    libraryDependencies ++= Seq(
      Config.typesafe,
      Tagging.supertagged
    )
  )
  .dependsOn(cakeless)
  .enablePlugins(MdocPlugin)

lazy val root = project
  .in(file("."))
  .aggregate(cakeless, `cakeless-injector`, `cakeless-compiletime`, `cakeless-ioc`, examples, docs)
  .settings(
    name := "cakeless-new-root",
    skip in publish := true,
    publish := {},
    publishLocal := {}
  )
  .withCommonSettings()
