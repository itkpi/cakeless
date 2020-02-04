package cakeless

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import zio._
import org.scalatest.{Assertion, WordSpec}
import zio.console._

class LifecycleSpec extends WordSpec {
  private val sampleWiring = SampleWiring(1)
  private val sampleZio    = ZIO.environment[SampleComponent].as(1)

  private val runtime = new DefaultRuntime {}

  def preStartSpec(): Assertion = {
    val reacted = new AtomicBoolean(false)
    val c: UIO[Int] =
      sampleZio.injectPrimary.withLifecycle {
        Lifecycle.preStart {
          ZIO.effectTotal {
            reacted.set(true)
          }
        }
      }.wire

    runtime.unsafeRun(c)

    assert(reacted.get())
  }

  "Lifecycle.preStart works correctly" in preStartSpec()

  def executesBeforeInstantiation(): Assertion = {
    val isBeforeInstantiation = new AtomicBoolean(true)

    trait SampleComponent2 {
      def foo: SampleWiring
      isBeforeInstantiation.set(true)
    }

    val c: UIO[Int] =
      ZIO
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

    runtime.unsafeRun(c)

    assert(isBeforeInstantiation.get())
  }

  "Lifecycle.preStart executes before instantiation" in executesBeforeInstantiation()

  def postStartSpec(): Assertion = {
    var reacted: Boolean = false
    val c: UIO[Int] =
      sampleZio.injectPrimary.withLifecycle {
        Lifecycle.postStart {
          ZIO.effectTotal {
            reacted = true
          }
        }
      }.wire

    runtime.unsafeRun(c)

    assert(reacted)
  }

  "Lifecycle.postStart works correctly" in postStartSpec()

  def executesAfterInstantiation(): Assertion = {
    val isAfterInstantiation = new AtomicBoolean(false)
    trait SampleComponent2 {
      def foo: SampleWiring
      isAfterInstantiation.set(false)
    }

    val c: UIO[Int] =
      ZIO
        .access[SampleComponent2](_.foo.foo)
        .injectPrimary
        .withLifecycle {
          Lifecycle.postStart(ZIO.effectTotal {
            isAfterInstantiation.set(true)
          })
        }
        .wire

    runtime.unsafeRun(c)

    assert(isAfterInstantiation.get())
  }

  "Lifecycle.postStart executes after instantiation" in executesAfterInstantiation()

  def componentUsage(): Assertion = {
    val flag          = new AtomicBoolean(false)
    val postUsedValue = new AtomicInteger(0)

    trait SampleComp2 {
      def x: SampleWiring

      println("Setting flag to true")
      flag.set(true)
    }

    val c: URIO[Console, Unit] =
      (ZIO.environment[SampleComp2] *> putStrLn("CREATED")).unit.injectPrimary
        .withLifecycle {
          Lifecycle.postStart((sw: SampleComp2) => ZIO.effectTotal(postUsedValue.set(sw.x.foo)))
        }
        .excludeZEnv[Console]
        .wire

    runtime.unsafeRun(c)
    assert(flag.get())
    assert(postUsedValue.get() == sampleWiring.foo)
    assert(flag.get())
  }

  "Lifecycle.postStart works correctly with component usage" in componentUsage()
}

trait SampleComponent {
  def foo: SampleWiring
}

case class SampleWiring(foo: Int)
