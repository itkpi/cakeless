package cakeless.lifecycle

import cakeless._
import cats.implicits._
import org.mockito.Mockito._
import org.scalatest.FlatSpec
import scala.util.Try

class LifecycleSpec extends FlatSpec {
  "Lifecycle.preStartF" should "work correctly" in {
    var reacted: Boolean = false
    val c = sampleCake.preStartF {
      reacted = true
      Try({})
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

    val c = cakeT[Try, SampleComponent2] preStartF {
      isBeforeInstantiation = false
      Try({})
    }

    c.as[SampleWiring] bake sampleWiring

    assert(isBeforeInstantiation)
  }

  "Lifecycle.preStart" should "work correctly" in {
    var reacted: Boolean = false
    val c = sampleCake.preStart {
      reacted = true
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

    val c = cakeT[Try, SampleComponent2] preStart {
      isBeforeInstantiation = false
    }

    c.as[SampleWiring] bake sampleWiring

    assert(isBeforeInstantiation)
  }

  "Lifecycle.postStartF" should "work correctly" in {
    var reacted: Boolean = false
    val c = sampleCake.postStartF {
      reacted = true
      Try({})
    }

    c bake sampleWiring

    assert(reacted)
  }

  it should "be executed after instantiation" in {
    var isAfterInstantiation: Boolean = true
    trait SampleComponent2 {
      def foo: Int
      isAfterInstantiation = false
    }

    val c = cakeT[Try, SampleComponent2] postStartF {
      isAfterInstantiation = true
      Try({})
    }

    c.as[SampleWiring] bake sampleWiring

    assert(isAfterInstantiation)
  }

  "Lifecycle.postStart" should "work correctly" in {
    var reacted: Boolean = false
    val c = sampleCake.postStart {
      reacted = true
      Try({})
    }

    c bake sampleWiring

    assert(reacted)
  }

  it should "be executed after instantiation" in {
    var isAfterInstantiation: Boolean = true
    trait SampleComponent2 {
      def foo: Int
      isAfterInstantiation = false
    }

    val c = cakeT[Try, SampleComponent2] postStart {
      isAfterInstantiation = true
    }

    c.as[SampleWiring] bake sampleWiring

    assert(isAfterInstantiation)
  }

  "Lifecycle.postStartUseF" should "work correctly" in {
    val mockFunc = getMockFunc
    when(mockFunc.apply)
    val c = sampleCake
      .map(_ => mockFunc)
      .postStartUseF { f =>
        Try(f())
      }

    c bake sampleWiring

    verify(mockFunc).apply()
  }

  "Lifecycle.postStartUse" should "work correctly" in {
    val mockFunc = getMockFunc
    when(mockFunc.apply)
    val c = sampleCake
      .map(_ => mockFunc)
      .postStartUse { f =>
        f()
      }

    c bake sampleWiring

    verify(mockFunc).apply()
  }

  "Lifecycle.handleErrorWith" should "work correctly" in {
    val c = sampleCake
      .map[Int](_ => throw new RuntimeException)
      .handleErrorWith { e =>
        Try(1 + 1)
      }

    val r = c bake sampleWiring

    assert(r.isSuccess)
    assert(r.get == 2)
  }

  "Lifecycle.handleError" should "work correctly" in {
    val c = sampleCake
      .map[Int](_ => throw new RuntimeException)
      .handleError { e =>
        2
      }

    val r = c bake sampleWiring

    assert(r.isSuccess)
    assert(r.get == 2)
  }

  "Lifecycle.recoverWith" should "work correctly" in {
    val c = sampleCake
      .map[Int](_ => throw new RuntimeException)
      .recoverWith {
        case e: RuntimeException =>
          Try(1 + 1)
      }

    val r = c bake sampleWiring

    assert(r.isSuccess)
    assert(r.get == 2)

    val c2 = sampleCake
      .map[Int](_ => throw new RuntimeException)
      .recoverWith {
        case e: IllegalArgumentException =>
          Try(1 + 1)
      }

    val r2 = c2 bake sampleWiring

    assert(r2.isFailure)
    assertThrows[RuntimeException](r2.get)
  }

  "Lifecycle.recover" should "work correctly" in {
    val c = sampleCake
      .map[Int](_ => throw new RuntimeException)
      .recover {
        case e: RuntimeException =>
          1 + 1
      }

    val r = c bake sampleWiring

    assert(r.isSuccess)
    assert(r.get == 2)

    val c2 = sampleCake
      .map[Int](_ => throw new RuntimeException)
      .recover {
        case e: IllegalArgumentException =>
          1 + 1
      }

    val r2 = c2 bake sampleWiring

    assert(r2.isFailure)
    assertThrows[RuntimeException](r2.get)
  }

  "Lifecycle.recoverNonFatal" should "work correctly" in {
    val c = sampleCake
      .map[Int](_ => throw new RuntimeException)
      .recoverNonFatal(1 + 1)

    val r = c bake sampleWiring

    assert(r.isSuccess)
    assert(r.get == 2)

    val c2 = sampleCake
      .map[Int](_ => throw new InterruptedException)
      .recoverNonFatal(1 + 1)

    assertThrows[InterruptedException] { c2 bake sampleWiring }
  }

  trait SampleComponent {
    def foo: Int
  }
  case class SampleWiring(foo: Int)

  private def getMockFunc  = mock(classOf[() => Unit])
  private val sampleWiring = SampleWiring(1)
  private val sampleCake   = cakeT[Try, SampleComponent].as[SampleWiring]
}
