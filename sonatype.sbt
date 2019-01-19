import xerial.sbt.Sonatype._

import scala.io.StdIn

val password = sys.env
  .get("SONATYPE_PASSWORD")
  .orElse(Option(System.getProperty("SONATYPE_PASSWORD")))
  .getOrElse(StdIn.readLine("Enter SONATYPE_PASSWORD: "))

credentials += Credentials(
  "Sonatype Nexus Repository Manager",
  "oss.sonatype.org",
  "vitalii-honta",
  password
)

sonatypeProfileName := "ua.pp.itkpi"

publishMavenStyle := true
// pgpPassphrase := Some(password.toCharArray)

licenses := Seq(
  "APL2" -> url("https://github.com/itkpi/cakeless/blob/master/LICENSE")
)

homepage := Some(url("https://github.com/itkpi/cakeless"))
scmInfo := Some(
  ScmInfo(
    url("https://github.com/itkpi/cakeless"),
    "scm:git@https://github.com/itkpi/cakeless.git"
  )
)

developers := List(
  Developer(
    id = "vitliihonta",
    name = "Vitalii Honta",
    email = "vitaliy.honta@gmail.com",
    url = url("https://github.com/vitaliihonta/")
  )
)

useGpg := false
pgpReadOnly := false
