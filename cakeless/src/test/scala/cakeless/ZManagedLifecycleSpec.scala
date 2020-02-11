package cakeless

import java.util.concurrent.atomic.AtomicBoolean
import zio._
import zio.console._
import zio.test.Assertion._
import zio.test._

object ZManagedLifecycleFixture {
  val sampleZManaged: ZManaged[SampleComponent, Nothing, Int] = ZManaged.environment[SampleComponent].as(1)
  val sampleDep: SampleDep                                    = SampleDep(1)

  def `Lifecycle.preStart works correctly`: UManaged[BoolAlgebra[FailureDetails]] =
    for {
      reacted <- ZManaged.fromEffect(Ref.make(false))
      result <- sampleZManaged.injectPrimary.withLifecycle {
        Lifecycle.preStart {
          reacted.set(true)
        }
      }.wire
      reactionResult <- ZManaged.fromEffect(reacted.get)
    } yield {
      assert(reactionResult, equalTo(true)) &&
      assert(result, equalTo(sampleDep.foo))
    }

  def `Lifecycle.preStart executes before instantiation`: UManaged[BoolAlgebra[FailureDetails]] = {
    val isBeforeInstantiation = new AtomicBoolean(true)

    trait SampleComponent2 {
      def foo: SampleDep
      isBeforeInstantiation.set(true)
    }

    for {
      result <- ZManaged
        .environment[SampleComponent2]
        .map(_.foo.foo)
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

  def `Lifecycle.postStart works correctly`: UManaged[BoolAlgebra[FailureDetails]] =
    for {
      reacted <- ZManaged.fromEffect(Ref.make(false))
      result <- sampleZManaged.injectPrimary.withLifecycle {
        Lifecycle.postStart {
          reacted.set(true)
        }
      }.wire
      reaction <- ZManaged.fromEffect(reacted.get)
    } yield {
      assert(reaction, equalTo(true)) &&
      assert(result, equalTo(sampleDep.foo))
    }

  def `Lifecycle.postStart executes after instantiation`: UManaged[BoolAlgebra[FailureDetails]] = {
    val isAfterInstantiation = new AtomicBoolean(false)
    trait SampleComponent2 {
      def foo: SampleDep
      isAfterInstantiation.set(false)
    }

    ZManaged
      .environment[SampleComponent2]
      .map(_.foo.foo)
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

  def `Lifecycle.postStart works correctly with component usage`: URManaged[Console, BoolAlgebra[FailureDetails]] = {
    val flag = new AtomicBoolean(false)

    trait SampleComp2 {
      def x: SampleDep

      println("Setting flag to true")
      flag.set(true)
    }

    for {
      postUsedValue <- ZManaged.fromEffect(Ref.make(0))
      _ <- (ZManaged.environment[SampleComp2] *> ZManaged.fromEffect(putStrLn("CREATED"))).unit.injectPrimary
        .withLifecycle {
          Lifecycle.postStart((sw: SampleComp2) => postUsedValue.set(sw.x.foo))
        }
        .excludeZEnv[Console]
        .wire
      resultedPostUsedValue <- ZManaged.fromEffect(postUsedValue.get)
    } yield {
      assert(flag.get(), equalTo(true)) &&
      assert(resultedPostUsedValue, equalTo(sampleDep.foo))
    }
  }
}

import cakeless.ZManagedLifecycleFixture._
object ZManagedLifecycleSpec
    extends DefaultRunnableSpec(
      suite("ZManaged")(
        testM("Lifecycle.preStart works correctly")(`Lifecycle.preStart works correctly`.use(ZIO.succeed)),
        testM("Lifecycle.preStart executes before instantiation")(`Lifecycle.preStart executes before instantiation`.use(ZIO.succeed)),
        testM("Lifecycle.postStart works correctly")(`Lifecycle.postStart works correctly`.use(ZIO.succeed)),
        testM("Lifecycle.postStart executes after instantiation")(`Lifecycle.postStart executes after instantiation`.use(ZIO.succeed)),
        testM("Lifecycle.postStart works correctly with component usage")(
          `Lifecycle.postStart works correctly with component usage`.use(ZIO.succeed)
        )
      )
    )
