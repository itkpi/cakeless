package cakeless

import java.util.concurrent.atomic.AtomicBoolean
import zio._
import zio.console._
import zio.test.Assertion._
import zio.test._

object ZioLifecycleFixture {
  val sampleZio: ZIO[SampleComponent, Nothing, Int] = ZIO.environment[SampleComponent].as(1)
  val sampleDep: SampleDep                          = SampleDep(1)

  def `Lifecycle.preStart works correctly`: UIO[BoolAlgebra[FailureDetails]] =
    for {
      reacted <- Ref.make(false)
      result <- sampleZio.injectPrimary.withLifecycle {
        Lifecycle.preStart {
          reacted.set(true)
        }
      }.wire
      reactionResult <- reacted.get
    } yield {
      assert(reactionResult, equalTo(true)) &&
      assert(result, equalTo(sampleDep.foo))
    }

  def `Lifecycle.preStart executes before instantiation`: UIO[BoolAlgebra[FailureDetails]] = {
    val isBeforeInstantiation = new AtomicBoolean(true)

    trait SampleComponent2 {
      def foo: SampleDep
      isBeforeInstantiation.set(true)
    }

    for {
      result <- ZIO
        .access[SampleComponent2](_.foo.foo)
        .injectPrimary
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
  }

  def `Lifecycle.postStart works correctly`: UIO[BoolAlgebra[FailureDetails]] =
    for {
      reacted <- Ref.make(false)
      result <- sampleZio.injectPrimary.withLifecycle {
        Lifecycle.postStart {
          reacted.set(true)
        }
      }.wire
      reaction <- reacted.get
    } yield {
      assert(reaction, equalTo(true)) &&
      assert(result, equalTo(sampleDep.foo))
    }

  def `Lifecycle.postStart executes after instantiation`: UIO[BoolAlgebra[FailureDetails]] = {
    val isAfterInstantiation = new AtomicBoolean(false)
    trait SampleComponent2 {
      def foo: SampleDep
      isAfterInstantiation.set(false)
    }

    ZIO
      .access[SampleComponent2](_.foo.foo)
      .injectPrimary
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
  }

  def `Lifecycle.postStart works correctly with component usage`: URIO[Console, BoolAlgebra[FailureDetails]] = {
    val flag = new AtomicBoolean(false)

    trait SampleComp2 {
      def x: SampleDep

      println("Setting flag to true")
      flag.set(true)
    }

    for {
      postUsedValue <- Ref.make(0)
      _ <- (ZIO.environment[SampleComp2] *> putStrLn("CREATED")).unit.injectPrimary
        .withLifecycle {
          Lifecycle.postStart((sw: SampleComp2) => postUsedValue.set(sw.x.foo))
        }
        .excludeZEnv[Console]
        .wire
      resultedPostUsedValue <- postUsedValue.get
    } yield {
      assert(flag.get(), equalTo(true)) &&
      assert(resultedPostUsedValue, equalTo(sampleDep.foo))
    }
  }
}

import cakeless.ZioLifecycleFixture._
object ZioLifecycleSpec
    extends DefaultRunnableSpec(
      suite("ZioLifecycleSpec")(
        testM("Lifecycle.preStart works correctly")(`Lifecycle.preStart works correctly`),
        testM("Lifecycle.preStart executes before instantiation")(`Lifecycle.preStart executes before instantiation`),
        testM("Lifecycle.postStart works correctly")(`Lifecycle.postStart works correctly`),
        testM("Lifecycle.postStart executes after instantiation")(`Lifecycle.postStart executes after instantiation`),
        testM("Lifecycle.postStart works correctly with component usage")(`Lifecycle.postStart works correctly with component usage`)
      )
    )
