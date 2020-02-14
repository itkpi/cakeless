package cakeless

import java.util.concurrent.atomic.AtomicBoolean
import zio._
import zio.console._
import zio.test.Assertion._
import zio.test._
import cakeless.ioc._
import cakeless.compiletime._

object ZioLifecycleFixture {
  val sampleZio: ZIO[SampleComponent, Nothing, Int] = ZIO.environment[SampleComponent].as(1)
  val sampleDep: SampleDep                          = SampleDep(1)
  val sampleConsole: Console.Service[Any]           = new Console.Live {}.console
}

import ZioLifecycleFixture._
object ZioLifecycleSpec
    extends DefaultRunnableSpec(
      suite("ZioLifecycleSpec")(
        testM("Lifecycle.preStart works correctly") {
          for {
            reacted <- Ref.make(false)
            result <- sampleZio.inject0.withLifecycle {
              Lifecycle.preStart {
                reacted.set(true)
              }
            }.wire
            reactionResult <- reacted.get
          } yield {
            assert(reactionResult, equalTo(true)) &&
            assert(result, equalTo(sampleDep.foo))
          }
        },
        testM("Lifecycle.preStart executes before instantiation") {
          val isBeforeInstantiation = new AtomicBoolean(true)

          trait SampleComponent2 {
            def foo: SampleDep
            isBeforeInstantiation.set(true)
          }

          for {
            result <- ZIO
              .access[SampleComponent2](_.foo.foo)
              .inject0
              .withLifecycle {
                Lifecycle.preStart {
                  ZIO.effectTotal {
                    isBeforeInstantiation.set(false)
                  }
                }
              }
              .wire
          } yield {
            assert(isBeforeInstantiation.get(), equalTo(true)) &&
            assert(result, equalTo(sampleDep.foo))
          }
        },
        testM("Lifecycle.postStart works correctly") {
          for {
            reacted <- Ref.make(false)
            result <- sampleZio.inject0.withLifecycle {
              Lifecycle.postStart {
                reacted.set(true)
              }
            }.wire
            reaction <- reacted.get
          } yield {
            assert(reaction, equalTo(true)) &&
            assert(result, equalTo(sampleDep.foo))
          }
        },
        testM("Lifecycle.postStart executes after instantiation") {
          val isAfterInstantiation = new AtomicBoolean(false)
          trait SampleComponent2 {
            def foo: SampleDep
            isAfterInstantiation.set(false)
          }

          ZIO
            .access[SampleComponent2](_.foo.foo)
            .inject0
            .withLifecycle {
              Lifecycle.postStart(ZIO.effectTotal {
                isAfterInstantiation.set(true)
              })
            }
            .wire
            .map { result =>
              assert(isAfterInstantiation.get(), equalTo(true)) &&
              assert(result, equalTo(sampleDep.foo))
            }
        },
        testM("Lifecycle.postStart works correctly with component usage") {
          val flag = new AtomicBoolean(false)

          trait SampleComp2 {
            def x: SampleDep

            println("Setting flag to true")
            flag.set(true)
          }

//          val cnsl: Console.Service[Any] = sampleConsole

          for {
            postUsedValue <- Ref.make(0)
            _ <- (ZIO
              .environment[SampleComp2] /**> putStrLn("CREATED")*/ )
              .unit
              .inject0
              .withLifecycle {
                Lifecycle.postStart((sw: SampleComp2) => postUsedValue.set(sw.x.foo))
              }
              //              .excludeZEnv[Console]
              .wire
            resultedPostUsedValue <- postUsedValue.get
          } yield {
            assert(flag.get(), equalTo(true)) &&
            assert(resultedPostUsedValue, equalTo(sampleDep.foo))
          }
        }
      )
    )
