---
Project: Cakeless
Current version: 1.0.0-rc3
Scala version: 2.12.10, 2.13.1
---

[![codecov](https://codecov.io/gh/itkpi/cakeless/branch/master/graph/badge.svg)](https://codecov.io/gh/itkpi/cakeless)
[![Build Status](https://travis-ci.com/itkpi/cakeless.svg?branch=master)](https://travis-ci.com/itkpi/cakeless)


# cakeless

Cakeless is a library providing the ability to painlessly wire deeply nested cakes into your ZIO environment.

To try it, add the following into your `build.sbt` file:
```scala
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= {
  val cakelessV = "1.0.0-rc3"
  Seq(
    "ua.pp.itkpi" %% "cakeless" % cakelessV
  )
}
```

Docs are available [here](./docs/readme.md)

Check examples [here](./examples/src/main/scala/com/examples)