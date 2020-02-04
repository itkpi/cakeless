lazy val snapshot: Boolean = false
isSnapshot in ThisBuild := snapshot
version in ThisBuild := {
  val vv = "1.0.0"
  if (!snapshot) vv
  else vv + "-SNAPSHOT"
}
