import xerial.sbt.Sonatype._

lazy val snapshot: Boolean = true
lazy val v: String = {
  val vv = "0.1"
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
    )

lazy val kernel = sonatypeProject(id = "cakeless", base = file("./kernel"))
  .settings(libraryDependencies ++= {
    val testV      = "3.0.4"
    val shapelessV = "2.3.3"
    Seq(
      "com.chuusai"   %% "shapeless" % shapelessV withSources (),
      "org.scalactic" %% "scalactic" % testV withSources (),
      "org.scalatest" %% "scalatest" % testV % "test" withSources ()
    )
  })

lazy val root = project
  .in(file("."))
  .aggregate(kernel)
  .settings(
    name := "cakeless",
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
