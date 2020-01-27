package cakeless

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import zio._
import org.scalatest.{Assertion, WordSpec}

class LifecycleSpec extends WordSpec {
  private val sampleWiring = SampleWiring(1)
  private val sampleZio   = ZIO.environment[SampleComponent].as(1)

  private val runtime = new DefaultRuntime {}

  def preStartSpec(): Assertion = {
    val reacted = new AtomicBoolean(false)
    val c: UIO[Int] = injectPrimary(
      sampleZio,
      Lifecycle.preStart(ZIO.effectTotal {
        reacted.set(true)
      })
    )

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

    val c: UIO[Int] = injectPrimary(
      ZIO.access[SampleComponent2](_.foo.foo),
      Lifecycle.preStart(ZIO.effectTotal {
        isBeforeInstantiation.set(false)
      })
    )

    runtime.unsafeRun(c)

    assert(isBeforeInstantiation.get())
  }

  "Lifecycle.preStart executes before instantiation" in executesBeforeInstantiation()

  def postStartSpec(): Assertion = {
    var reacted: Boolean = false
    val c: UIO[Int] = injectPrimary(
      sampleZio,
      Lifecycle.postStart(ZIO.effectTotal {
        reacted = true
      })
    )

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

    val c: UIO[Int] = injectPrimary(
      ZIO.access[SampleComponent2](_.foo.foo),
      Lifecycle.postStart(ZIO.effectTotal {
        isAfterInstantiation.set(true)
      })
    )

    runtime.unsafeRun(c)

    assert(isAfterInstantiation.get())
  }

  "Lifecycle.postStart executes after instantiation" in executesAfterInstantiation()

  def componentUsage(): Assertion = {
    val flag = new AtomicBoolean(false)
    val postUsedValue = new AtomicInteger(0)
    val c: UIO[Boolean] = injectPrimary(
      sampleZio.map(_ => flag.get()),
      Lifecycle.postStart((sw: SampleComponent) =>
        ZIO.effectTotal(flag.set(true)) *>
          ZIO.effectTotal(postUsedValue.set(sw.foo.foo))
      )
    )

    assert(!runtime.unsafeRun(c))
    assert(postUsedValue.get() == sampleWiring.foo)
    assert(flag.get())
  }

  "Lifecycle.postStart works correctly with component usage" in componentUsage()
}

trait SampleComponent {
  def foo: SampleWiring
}

case class SampleWiring(foo: Int)
