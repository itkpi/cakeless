# Cakeless

The main goal of `cakeless` is to provide lightweight DI capabilities for `ZIO` environment.
It also supports `ZManaged` wiring and potentially any data structure provided by `zio`  
which has shape `(-R, +E, +A) => Z[R, E, A]`.

## Getting started

Imagine you decided to use cake-pattern as your IOC.
First, it is strongly recommended to use tagged types for clarity.
I recommend using [supertagged](https://github.com/rudogma/scala-supertagged)
Let's start with the definition of your tagged types:

```scala
val supertaggedVersion = "<latest-version>"
libraryDependencies += "org.rudogma" %% "supertagged" % supertaggedVersion
```


### Preparation

Then you can use this simple library with no runtime allocations for your tagged types:

```scala mdoc
import java.nio.file.Path
import supertagged._

object types {
  object ConfigPath extends TaggedType[Path]
  type ConfigPath = ConfigPath.Type

  object Props extends TaggedType[Map[String, String]]
  type Props = Props.Type

  object Token extends TaggedType[String]
  type Token = Token.Type

  object Username extends TaggedType[String]
  type Username = Username.Type

  object Password extends TaggedType[String]
  type Password = Password.Type

  object DbUrl extends TaggedType[String]
  type DbUrl = DbUrl.Type
}
```

Finally, let's define our components using cake-pattern.  
Some basic stuff:

```scala mdoc

import types._
import scala.concurrent.ExecutionContext

trait ExecutionContextComponent {
  implicit def ec: ExecutionContext
}

trait FileConfigComponent {
  def configPath: ConfigPath
}

trait PropsComponent {
  def props: Props
}
```

Very simple.
Then let's define some nested cakes:

```scala mdoc

import zio._
import com.typesafe.config.{Config, ConfigException, ConfigFactory}

trait AllComponents1 {
  self: FileConfigComponent =>

  def getConfigFile: IO[ConfigException, Config] =
    IO.effect {
        ConfigFactory.parseFile(configPath.toFile)
      }
      .refineToOrDie[ConfigException]
}
```

... Something more complicated

```scala mdoc

import zio._
import scala.concurrent.Future

trait AllComponents2 { self: ExecutionContextComponent with PropsComponent =>
  def getProp(prop: String): UIO[Option[String]] = {
    def legacyFutureCode = Future {
      props get prop
    }

    ZIO.fromFuture { _ =>
      legacyFutureCode
    }.orDie
  }
}
```

... And even mixed constructor-based IOC (with alternative constructors) and cake pattern!

```scala mdoc

import zio._
import types._

class NestedComponent(implicit val token: Token) {
  self: AllComponents2 with ExecutionContextComponent with PropsComponent =>

  def this(username: Username, password: Password) =
    this()(Token(s"$username@$password"))

  def getConfig(config: Config)(key: String): UIO[Option[String]] =
    IO.effect {
      config getString key
    }.option
}
```


### Declaring your program

Having such a domain model we can now create some useful programs:

```scala mdoc

import zio._

object MyProgram {
    val configValue =
      ZIO
        .accessM[AllComponents1] { c =>
          c.getConfigFile
        }
        .mapError(e => new IllegalArgumentException("Config missing", e))
    
    val component2 = ZIO.environment[NestedComponent with AllComponents2]
    
    val hostValue = component2.flatMap { c =>
      c.getProp("host").flatMap {
        case Some(host) => ZIO.succeed(host)
        case None       => ZIO.fail(new IllegalArgumentException("host not found"))
      }
    }
    
    val url: ZIO[NestedComponent with AllComponents2 with AllComponents1, IllegalArgumentException, String] = for {
      config <- configValue
      c2     <- component2
      port   <- c2.getConfig(config)("http.port").map(_.getOrElse(80))
      host   <- hostValue
    } yield s"$host:$port?token=${c2.token}"
}
```

### Manual wiring

Let's try to provide ZIO environment without cakeless:

```scala mdoc

import zio.{ App => ZApp,  _ }
import zio.console._
import scala.concurrent.ExecutionContext
import types._
import java.nio.file.Paths

object Program extends ZApp {
    def run(args: List[String]) = {
        val username: Username         = Username("vitaliihonta")
        val password: Password         = Password("password")
        val propsProd: Props           = Props(Map("host" -> "4.4.4.4"))
        val configPathProd: ConfigPath = ConfigPath(Paths.get("./examples/src/main/resources/application.conf"))

        val wired: IO[IllegalArgumentException, String] = MyProgram.url.provide {
            new NestedComponent(username, password) // alternative constructor
              with AllComponents2
              with AllComponents1
              with ExecutionContextComponent
              with PropsComponent
              with FileConfigComponent {
            
              override implicit val ec: ExecutionContext = ExecutionContext.global
              override val props: Props                  = propsProd
              override val configPath: ConfigPath        = configPathProd
            }
        }
        wired
            .catchAll(e => ZIO.succeed(e.getMessage))
            .flatMap(putStrLn) *> ZIO.succeed(0)
    }
}
```

### Wiring with cakeless
Instead cakeless allows to do it easier:

```scala mdoc

import zio.{ App => ZApp, _ }
import scala.concurrent.ExecutionContext 
import java.nio.file.Path
import cakeless._
import cakeless.nat._
import types._

object CakelessProgram extends ZApp {
    def run(args: List[String]) = {
        val username: Username         = Username("vitaliihonta")
        val password: Password         = Password("password")
        val propsProd: Props           = Props(Map("host" -> "4.4.4.4"))
        val configPathProd: ConfigPath = ConfigPath(Paths.get("./examples/src/main/resources/application.conf"))
        import ExecutionContext.Implicits.global // implicit search is left for scalac

        val wired: IO[IllegalArgumentException, String] = MyProgram.url.inject[_1].wire
        wired
            .catchAll(e => ZIO.succeed(e.getMessage))
            .flatMap(putStrLn) *> ZIO.succeed(0)
    }
}
```

Much more simple, isn't it?)
1) You don't need to refer to `NestedComponent` self-type (e.g. its underlying structure)
2) You don't need to annotate anything (in most cases)
3) Choosing constructor is just replacing `_1` with `_2`, etc. or just calling `injectPrimary` instead of `inject[N]`
4) **TODO**: try to comment some of the dependencies and see what happens  
(spoiler: in most cases its concise compile-time errors)

### Lifecycle capabilities

```scala mdoc

import zio.{ App => ZApp , _ }
import zio.console._
import scala.concurrent.ExecutionContext 
import cakeless._
import types._
import java.nio.file.Paths

object MyProgramWithLifecycle extends ZApp {
    def run(args: List[String]) = {
        val configPathImpl: ConfigPath = ConfigPath(Paths.get("./examples/src/main/resources/application.conf"))

        ZIO
            .accessM[AllComponents1](_.getConfigFile)
            .tap(c => putStrLn(c.toString))
            .injectPrimary
            .withLifecycle(
                Lifecycle.preStart(
                    putStrLn("Component 1 preStart") // this code will be ran before AllComponents1 
                ) && Lifecycle.postStart(
                    putStrLn("Component 1 started!") // this code will be ran before AllComponents1 
                )
            )
            .excludeZEnv[Console] // Console is a side-effect, lets allow zio.App to provide it for us
            .wire
            .fold(_ => 1, _ => 0)
    }
}
```

So:
1) You can ask cakeless to run arbitrary effects *before* `AllComponents1` initialization (using `Lifecycle.preStart`)
2) You can ask cakeless to run arbitrary effects *after* `AllComponents1` got initialized (using `Lifecycle.postStart` with 2 overloaded alternatives)
3) You can chain lifecycle using `&&` operator

### ZManaged support
The injection mechanism for `ZManaged` is exactly like in the examples above.
See examples directory for more details

### Other features

1) Name collision detection will cause compile-time errors (when provided `val`s name is the same as some `def` name deep in the cake,  
it will cause StackOverflowError because of cyclic reference) 

2) When tagged types don't rescue and you have multiple dependency instances of the same type,  
you may use `@wired` annotation to provide a hint for `cakeless`

