package cakeless.lifecycle

import java.util.concurrent.atomic.AtomicBoolean
import cakeless._
import scalaz.zio._
import org.scalatest.FlatSpec

class LifecycleSpec extends FlatSpec {
  private val runtime = new DefaultRuntime {}

  "Lifecycle.preStartF" should "work correctly" in {
    var reacted: Boolean = false
    val c = sampleCake.preStartF {
      reacted = true
      ZIO.unit
    }

    c bake sampleWiring

    assert(reacted)
  }

  it should "be executed before instantiation" in {
    var isBeforeInstantiation: Boolean = true
    trait SampleComponent2 {
      def foo: Int
      isBeforeInstantiation = true
    }

    val c = cakeZ[Any, Throwable, SampleComponent2] preStartF {
      isBeforeInstantiation = false
      ZIO.unit[Any]
    }

    runtime.unsafeRun(c.as[SampleWiring] bake sampleWiring)

    assert(isBeforeInstantiation)
  }

  "Lifecycle.preStart" should "work correctly" in {
    var reacted: Boolean = false
    val c = sampleCake.preStart {
      reacted = true
    }

    runtime.unsafeRun(c bake sampleWiring)

    assert(reacted)
  }

  it should "be executed before instantiation" in {
    var isBeforeInstantiation: Boolean = true
    trait SampleComponent2 {
      def foo: Int
      isBeforeInstantiation = true
    }

    val c = cakeZ[Any, Throwable, SampleComponent2] preStart {
      isBeforeInstantiation = false
    }

    runtime.unsafeRun(c.as[SampleWiring] bake sampleWiring)

    assert(isBeforeInstantiation)
  }

  "Lifecycle.postStartF" should "work correctly" in {
    var reacted: Boolean = false
    val c = sampleCake.postStartF {
      reacted = true
      ZIO.unit[Any]
    }

    runtime.unsafeRun(c bake sampleWiring)

    assert(reacted)
  }

  it should "be executed after instantiation" in {
    var isAfterInstantiation: Boolean = true
    trait SampleComponent2 {
      def foo: Int
      isAfterInstantiation = false
    }

    val c = cakeZ[Any, Throwable, SampleComponent2] postStartF {
      ZIO(isAfterInstantiation = true)
    }

    runtime.unsafeRun(c.as[SampleWiring] bake sampleWiring)

    assert(isAfterInstantiation)
  }

  "Lifecycle.postStart" should "work correctly" in {
    var reacted: Boolean = false
    val c = sampleCake.postStart {
      reacted = true
    }

    runtime.unsafeRun(c bake sampleWiring)

    assert(reacted)
  }

  it should "be executed after instantiation" in {
    var isAfterInstantiation: Boolean = true
    trait SampleComponent2 {
      def foo: Int
      isAfterInstantiation = false
    }

    val c = cakeZ[Any, Throwable, SampleComponent2] postStart {
      isAfterInstantiation = true
    }

    runtime.unsafeRun(c.as[SampleWiring] bake sampleWiring)

    assert(isAfterInstantiation)
  }

  "Lifecycle.postStartUseF" should "work correctly" in {
    val flag = new AtomicBoolean(false)
    val c = sampleCake
      .map(_ => flag)
      .postStartUseF { f =>
        ZIO(f.set(true))
      }

    runtime.unsafeRun(c bake sampleWiring)
    assert(flag.get())
  }

  "Lifecycle.postStartUse" should "work correctly" in {
    val flag = new AtomicBoolean(false)
    val c = sampleCake
      .map(_ => flag)
      .postStartUse { f =>
        flag.set(true)
      }

    runtime.unsafeRun(c bake sampleWiring)

    assert(flag.get())
  }

  "Lifecycle.catchAllWith" should "work correctly" in {
    val c = sampleCake
      .mapM(_ => ZIO.fail(new RuntimeException))
      .catchAllWith { e =>
        ZIO(1 + 1)
      }

    val r = runtime.unsafeRun(c bake sampleWiring)

    assert(r == 2)
  }

  "Lifecycle.catchAll" should "work correctly" in {
    val c = sampleCake
      .mapM(_ => ZIO.fail(new RuntimeException))
      .catchAll { e =>
        2
      }

    val r = runtime.unsafeRun(c bake sampleWiring)

    assert(r == 2)
  }

  "Lifecycle.catchSomeWith" should "work correctly" in {
    val c = sampleCake
      .mapM(_ => ZIO.fail(new RuntimeException))
      .catchSomeWith {
        case e: RuntimeException =>
          ZIO(1 + 1)
      }

    val r = runtime.unsafeRun(c bake sampleWiring)

    assert(r == 2)

    val c2 = sampleCake
      .mapM(_ => ZIO.fail(new RuntimeException))
      .catchSomeWith {
        case e: IllegalArgumentException =>
          ZIO(1 + 1)
      }

    val r2 = c2 bake sampleWiring

    assertThrows[FiberFailure] {
      runtime.unsafeRun(r2)
    }
  }

  "Lifecycle.catchSome" should "work correctly" in {
    val c = sampleCake
      .mapM(_ => ZIO.fail(new RuntimeException))
      .catchSome {
        case e: RuntimeException =>
          1 + 1
      }

    val r = runtime.unsafeRun(c bake sampleWiring)

    assert(r == 2)

    val c2 = sampleCake
      .mapM(_ => ZIO.fail(new RuntimeException))
      .catchSome {
        case e: IllegalArgumentException =>
          1 + 1
      }

    val r2 = c2 bake sampleWiring

    assertThrows[FiberFailure] {
      runtime.unsafeRun(r2)
    }
  }

  trait SampleComponent {
    def foo: Int
  }
  case class SampleWiring(foo: Int)

  private val sampleWiring = SampleWiring(1)
  private val sampleCake   = cakeZ[Any, Throwable, SampleComponent].as[SampleWiring]
}
