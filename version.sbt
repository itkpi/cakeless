lazy val snapshot: Boolean = true
isSnapshot in ThisBuild := snapshot
version in ThisBuild := {
  val vv = "1.0.0-rc3"
  if (!snapshot) vv
  else vv + "-SNAPSHOT"
}
