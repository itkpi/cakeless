---
Project: Cakeless
Current version: 0.4.0-SNAPSHOT
Scala version: 2.12.8
Interop: Cats, ZIO
---

[![codecov](https://codecov.io/gh/itkpi/cakeless/branch/master/graph/badge.svg)](https://codecov.io/gh/itkpi/cakeless)
[![Build Status](https://travis-ci.com/itkpi/cakeless.svg?branch=master)](https://travis-ci.com/itkpi/cakeless)

![Cats Friendly Badge](https://typelevel.org/cats/img/cats-badge-tiny.png) 

# cakeless

Cakeless is library providing better reader monad for well-known cake pattern.
It has 2 separate implementations (both depend on shapeless):
- cakeless-cats - implemented using [cats-effect](https://github.com/typelevel/cats-effect)
- cakeless-zio  - implemented using [scalaz-zio](https://scalaz.github.io/scalaz-zio/)

Both implementations share the same kernel and common functionality.

The difference is in naming convention (`recover` vs `catchSome`, etc.) and data type.
For instance, cats implementation provides `CakeT[F[_], A]` (where F is arbitrary Monad)
On the other hand, zio implementation provides `CakeZ[Effect, Error, A]` with more control over your effects and errors

Both implementations allows to wire your dependencies by hands and automatically (like macwire does)

To try it, add the following into your `build.sbt` file:
```scala
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= {
  val cakelessV = "0.4.0-SNAPSHOT"
  Seq(
    "ua.pp.itkpi" %% "cakeless-cats" % cakelessV
    // or "ua.pp.itkpi" %% "cakeless-zio" % cakelessV
  )
}
```

Assume you have such components:
```scala
import scala.concurrent.ExecutionContext
import java.nio.file.Path


trait ExecutionContextComponent {
  implicit def ec: ExecutionContext
}

trait FileConfigComponent {
  def configPath: Path
}
```

Using cakeless you can lift those components into `Cake` monad:

```scala
import cakaless._
import shapeless.{::, HNil}


val cake0: Cake.Aux[ExecutionContextComponent with FileConfigComponent, ExecutionContext :: Path :: HNil] = 
  cake[ExecutionContextComponent with FileConfigComponent]
```
As you can see cakeless automaticaly infers abstract `def`s and `val`s (e.g. dependencies that should be provided) and picks them up into type definition (as `HList`).

You can also lift another cake (it can't be too much cakes):

```scala
trait PropsComponent {
  def props: Map[String, String]
}

// component with self types
trait AllComponents2 { self: ExecutionContextComponent with PropsComponent =>
  def getPropAsync(prop: String): Future[Option[String]] = Future {
    props get prop
  }
}

...

val cake1: Cake.Aux[AllComponents2 with ExecutionContextComponent with PropsComponent, ExecutionContext :: Map[String, String] :: HNil] = 
  cake[AllComponents2 with ExecutionContextComponent with PropsComponent]
```

Than you can `flatMap` over cake:
```scala
val program: Cake.Aux[String, ExecutionContext :: Path :: Map[String, String] :: HNil] = 
  for {
    c0 <- cake0
    c1 <- cake1
  } yield "foo"
```

Cakeless will merge its dependencies using `shapeless.ops.hlist.Union` so that:
1) Dependencies from different cakes will be accumulated along with computaitons
2) Repeating dependencies (like 2 `ExecutionContext`s) will be merged into 1

You can then bake the cake:
```scala
val result: String = program.bake(ExecutionContext.global :: Paths.get("foo") :: Map("foo" -> "bar") :: HNil)

// or using case class so you can name dependencies explicitly
case class Wiring(ec: ExecutionContext, configPath: Path, props: Map[String, String])

// by hands
val result1: String = program bake Wiring(
  ec = ExecutionContext.global,
  configPath = Paths.get("foo"),
  props = Map("foo" -> "bar")
)

// or automatically when dependencies are defined in scope
val ec = ExecutionContext.global
val configPath = Paths.get("foo")
val props = Map("foo" -> "bar")

val result2: String = program.auto
```

For full example with `cats.effect` see [here](examples-cats/src/main/scala/com/examples/Basic.scala)

For full example with `zio` see [here](examples-zio/src/main/scala/com/examples/Basic.scala)

For automatic wiring samples see:
[cats auto](examples-cats/src/main/scala/com/examples/AutoSample.scala)
[zio auto](examples-zio/src/main/scala/com/examples/AutoSample.scala)

# Lifecycle management integrations
See [cats example](examples-cats/src/main/scala/com/examples/LifecycleExample.scala)

See [zio example](examples-zio/src/main/scala/com/examples/LifecycleExample.scala)
