package cakeless

import cats._
import cats.implicits._
import cats.data.WriterT
import org.scalatest.FlatSpec
import shapeless.{::, HNil}
import cakeless.internal.{UnUnion, Union}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Try

class KernelSpec extends FlatSpec {
  trait FooComponent {
    def foo: Int
  }

  trait BarComponent {
    def bar: String
  }

  case class FooBarWiring(foo: Int, bar: String)

  trait FooBarComponent { self: FooComponent with BarComponent =>
    def fooBar: String = foo + bar
  }

  trait BazComponent {
    def baz: Double
  }

  trait BarBazComponent { self: BarComponent with BazComponent =>
    def barBaz: String = bar + baz
  }

  case class BarBazWiring(bar: String, baz: Double)

  case class FooBarBazWiring(foo: Int, bar: String, baz: Double)

  "Union" should "work for case classes" in {
    assertCompiles("""implicitly[Union.Aux[FooBarWiring, BarBazWiring, FooBarBazWiring]]""")
  }

  "UnUnion" should "work correctly" in {
    val a: Int :: String :: HNil               = 1 :: "foo" :: HNil
    val b: String :: Double :: HNil            = "foo" :: 2.0 :: HNil
    val union: Int :: String :: Double :: HNil = a union b

    val unUnion = UnUnion[Int :: String :: HNil, String :: Double :: HNil, Int :: String :: Double :: HNil]

    val (ax, bx) = unUnion(union)
    assert(a == ax)
    assert(b == bx)
  }

  "cake for simple component" should "compile" in {
    val c1 = cake[FooComponent]

    assertCompiles("""implicitly[c1.Dependencies <:< (Int :: HNil)]""")

    val c2 = cake[FooComponent with BarComponent]
    assertCompiles("""implicitly[c2.Dependencies <:< (Int :: String :: HNil)]""")
  }

  it should "work correctly" in {
    val c1     = cake[FooComponent]
    val result = c1.bake(1 :: HNil)

    assert(result.foo == 1)

    val c2      = cake[FooComponent with BarComponent]
    val result2 = c2.bake(2 :: "bar" :: HNil)

    assert(result2.foo == 2)
    assert(result2.bar == "bar")
  }

  "cake for nested components" should "compile" in {
    val c1 = cake[FooBarComponent with FooComponent with BarComponent]
    assertCompiles("""implicitly[c1.Dependencies <:< (Int :: String :: HNil)]""")

    val c2 = cake[BarBazComponent with BarComponent with BazComponent]
    assertCompiles("""implicitly[c2.Dependencies <:< (String :: Double :: HNil)]""")
  }

  it should "work correctly" in {
    val c1      = cake[FooBarComponent with FooComponent with BarComponent]
    val result0 = c1.bake(1 :: "foo" :: HNil)

    assert(result0.foo == 1)
    assert(result0.bar == "foo")
    assert(result0.fooBar == "1foo")

    val c2      = cake[BarBazComponent with BarComponent with BazComponent]
    val result1 = c2.bake("bar" :: 2.0 :: HNil)

    assert(result1.bar == "bar")
    assert(result1.baz == 2.0)
    assert(result1.barBaz == "bar2.0")
  }

  "cake.bake with Generic" should "work correctly" in {
    val c2      = cake[BarBazComponent with BarComponent with BazComponent]
    val result1 = c2.as[BarBazWiring] bake BarBazWiring(bar = "bar", baz = 2.0)

    assert(result1.bar == "bar")
    assert(result1.baz == 2.0)
  }

  "cake.map" should "work correctly" in {
    val c1      = cake[FooComponent].map(_.foo + 2)
    val result1 = c1.bake(1 :: HNil)
    assert(result1 == 3)
  }

  "cake.comap" should "work correctly" in {
    val c1      = cake[FooComponent].comap[String](s => s.toInt :: HNil)
    val result1 = c1.bake("10")
    assert(result1.foo == 10)
  }

  "cake.flatMap" should "accumulate depedencies on the typelevel" in {
    val c1 = cake[FooBarComponent with FooComponent with BarComponent]
    val c2 = cake[BarBazComponent with BarComponent with BazComponent]

    val c3 = for {
      c1 <- c1
      c2 <- c2
    } yield {}

    assertCompiles("""implicitly[c3.Dependencies <:< (Int :: String :: Double :: HNil)]""")
  }

  it should "work correctly" in {
    val c1 = cake[FooBarComponent with FooComponent with BarComponent]
    val c2 = cake[BarBazComponent with BarComponent with BazComponent]

    val c3 = for {
      c1 <- c1
      c2 <- c2
    } yield (c1, c2)

    val (comp1, comp2) = c3.bake(1 :: "foo" :: 2.0 :: HNil)

    assert(comp1.foo == 1)
    assert(comp1.bar == "foo")
    assert(comp2.bar == comp1.bar)
    assert(comp2.baz == 2.0)
  }

  "cake" should "be correctly converted into Reader" in {
    val c1 = cake[FooBarComponent with FooComponent with BarComponent]

    val reader = c1.toReader
    val result = reader.run(1 :: "foo" :: HNil)
    assert(result.foo == 1)
    assert(result.bar == "foo")
  }

  it should "be correctly converted into Reader using shapeless.Generic" in {
    val c1 = cake[FooBarComponent with FooComponent with BarComponent]

    val reader = c1.as[FooBarWiring].toReader
    val result = reader.run(FooBarWiring(foo = 1, bar = "foo"))
    assert(result.foo == 1)
    assert(result.bar == "foo")
  }

  it should "keep logs using WriterT" in {
    val c1 = cake[FooBarComponent with FooComponent with BarComponent]
    val c2 = cake[BarBazComponent with BarComponent with BazComponent]

    val creatingFooBar = "Creating FooBarComponent"
    val creatingBarBaz = "Creating BarBazComponent"

    val c3 = for {
      c1 <- c1 logged List(creatingFooBar)
      c2 <- c2 logged List(creatingBarBaz)
    } yield (c1, c2)

    val result = c3.bake(1 :: "foo" :: 2.0 :: HNil)

    val (log, (comp1, comp2)) = result.run

    assert(log == List(creatingFooBar, creatingBarBaz))
    assert(comp1.foo == 1)
    assert(comp1.bar == "foo")
    assert(comp2.bar == comp1.bar)
    assert(comp2.baz == 2.0)
  }

  "cake.mapK" should "work correctly" in {
    val c1   = cake[FooBarComponent with FooComponent with BarComponent]
    val cTry = c1.mapK[Try](λ[Id[?] ~> Try[?]](Try(_)))

    val result = cTry.as[FooBarWiring] bake FooBarWiring(foo = 1, bar = "foo")

    assert(result.isSuccess)
    assert(result.get.foo == 1)
    assert(result.get.bar == "foo")
  }

  "cakeT" should "be automatically derived for any applicative" in {
    import scala.concurrent.ExecutionContext.Implicits.global

    val c1 = cakeT[Try, FooBarComponent with FooComponent with BarComponent]
    val c2 = cakeT[WriterT[Future, List[String], ?], BarBazComponent with BarComponent with BazComponent]

    val c11 = c1.mapK[Future](λ[Try[?] ~> Future[?]](Future.fromTry(_)))

    val program = for {
      c1 <- c11 logged List("foo")
      c2 <- c2
    } yield c1.fooBar + c2.barBaz

    val writer = program.as[FooBarBazWiring] bake FooBarBazWiring(foo = 1, bar = "foo", baz = 2.0)

    val (log, result) = Await.result(writer.run, Duration.Inf)

    assert(log == List("foo"))
    assert(result == "1foofoo2.0")
  }
}
