package cakeless

import java.util.concurrent.atomic.AtomicBoolean

import cakeless.ZioLifecycleFixture.sampleConsole
import zio._
import zio.console._
import cakeless.ioc._
import cakeless.compiletime._
import zio.test.Assertion._
import zio.test._

object ZManagedLifecycleFixture {
  val sampleZManaged: ZManaged[SampleComponent, Nothing, Int] = ZManaged.environment[SampleComponent].as(1)
  val sampleDep: SampleDep                                    = SampleDep(1)
}

import cakeless.ZManagedLifecycleFixture._
object ZManagedLifecycleSpec
    extends DefaultRunnableSpec(
      suite("ZManaged")(
        testM("Lifecycle.preStart works correctly") {
          (for {
            reacted <- ZManaged.fromEffect(Ref.make(false))
            result <- sampleZManaged.inject0.withLifecycle {
              Lifecycle.preStart {
                reacted.set(true)
              }
            }.wire
            reactionResult <- ZManaged.fromEffect(reacted.get)
          } yield {
            assert(reactionResult, equalTo(true)) &&
            assert(result, equalTo(sampleDep.foo))
          }).use(ZIO.succeed)
        },
        testM("Lifecycle.preStart executes before instantiation") {
          val isBeforeInstantiation = new AtomicBoolean(true)

          trait SampleComponent2 {
            def foo: SampleDep
            isBeforeInstantiation.set(true)
          }

          (for {
            result <- ZManaged
              .environment[SampleComponent2]
              .map(_.foo.foo)
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
          }).use(ZIO.succeed)
        },
        testM("Lifecycle.postStart works correctly") {
          (for {
            reacted <- ZManaged.fromEffect(Ref.make(false))
            result <- sampleZManaged.inject0.withLifecycle {
              Lifecycle.postStart {
                reacted.set(true)
              }
            }.wire
            reaction <- ZManaged.fromEffect(reacted.get)
          } yield {
            assert(reaction, equalTo(true)) &&
            assert(result, equalTo(sampleDep.foo))
          }).use(ZIO.succeed)
        },
        testM("Lifecycle.postStart executes after instantiation") {
          {
            val isAfterInstantiation = new AtomicBoolean(false)
            trait SampleComponent2 {
              def foo: SampleDep
              isAfterInstantiation.set(false)
            }

            ZManaged
              .environment[SampleComponent2]
              .map(_.foo.foo)
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
              .use(ZIO.succeed)
          }
        },
        testM("Lifecycle.postStart works correctly with component usage") {
          val flag = new AtomicBoolean(false)

          trait SampleComp2 {
            def x: SampleDep

            println("Setting flag to true")
            flag.set(true)
          }

          (for {
            postUsedValue <- ZManaged.fromEffect(Ref.make(0))
            _ <- {
//              val cnsl: Console.Service[Any] = sampleConsole
              (ZManaged
                .environment[SampleComp2] /* *> ZManaged.fromEffect(putStrLn("CREATED"))*/ )
                .unit
                .inject0
                .withLifecycle {
                  Lifecycle.postStart((sw: SampleComp2) => postUsedValue.set(sw.x.foo))
                }
                //                .excludeZEnv[Console]
                .wire
            }
            resultedPostUsedValue <- ZManaged.fromEffect(postUsedValue.get)
          } yield {
            assert(flag.get(), equalTo(true)) &&
            assert(resultedPostUsedValue, equalTo(sampleDep.foo))
          }).use(ZIO.succeed)
        }
      )
    )
