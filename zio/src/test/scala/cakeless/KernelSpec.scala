package cakeless

import cakeless.internal.UnUnion
import cakeless.tagging._
import org.scalatest.FlatSpec
import scalaz.zio._
import shapeless.{::, HNil, Witness}

class KernelSpec extends FlatSpec {
  type foo = Witness.`"foo"`.T

  private val runtime = new DefaultRuntime {}

  case class FooWiringConcrete(foo: Int @@ foo)

  trait FooComponent {
    def foo: Int
  }

  trait BarComponent {
    def bar: String
  }

  case class FooBarWiring(foo: Int, bar: String)

  trait FooBarComponent {
    self: FooComponent with BarComponent =>
    def fooBar: String = foo + bar
  }

  trait BazComponent {
    def baz: Double
  }

  trait BarBazComponent {
    self: BarComponent with BazComponent =>
    def barBaz: String = bar + baz
  }

  class ComponentWithConstructors(val bg: BigInt) {
    self: FooComponent =>
    def this(l: Long) = this(BigInt(l))
  }

  case class BarBazWiring(bar: String, baz: Double)

  case class FooBarBazWiring(foo: Int, bar: String, baz: Double)

  "Union" should "work for case classes" in {
    assertCompiles("""implicitly[cakeless.internal.Union.Aux[FooBarWiring, BarBazWiring, FooBarBazWiring]]""")
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
    val c1 = cakeZ[FooComponent]

    assertCompiles("""implicitly[c1.Dependencies <:< (Int :: HNil)]""")

    val c2 = cakeZ[FooComponent with BarComponent]
    assertCompiles("""implicitly[c2.Dependencies <:< (Int :: String :: HNil)]""")
  }

  it should "work correctly" in {
    val c1     = cakeZ[FooComponent]
    val result = c1.bake(1 :: HNil)

    assert(runtime.unsafeRun(result).foo == 1)

    val c2      = cakeZ[FooComponent with BarComponent]
    val result2 = c2.bake(2 :: "bar" :: HNil)

    assert(runtime.unsafeRun(result2).foo == 2)
    assert(runtime.unsafeRun(result2).bar == "bar")
  }

  "cakeZ for nested components" should "compile" in {
    val c1 = cakeZ[FooBarComponent with FooComponent with BarComponent]
    assertCompiles("""implicitly[c1.Dependencies <:< (Int :: String :: HNil)]""")

    val c2 = cakeZ[BarBazComponent with BarComponent with BazComponent]
    assertCompiles("""implicitly[c2.Dependencies <:< (String :: Double :: HNil)]""")
  }

  it should "work correctly" in {
    val c1      = cakeZ[FooBarComponent with FooComponent with BarComponent]
    val result0 = c1.bake(1 :: "foo" :: HNil)

    assert(runtime.unsafeRun(result0).foo == 1)
    assert(runtime.unsafeRun(result0).bar == "foo")
    assert(runtime.unsafeRun(result0).fooBar == "1foo")

    val c2      = cakeZ[BarBazComponent with BarComponent with BazComponent]
    val result1 = c2.bake("bar" :: 2.0 :: HNil)

    assert(runtime.unsafeRun(result1).bar == "bar")
    assert(runtime.unsafeRun(result1).baz == 2.0)
    assert(runtime.unsafeRun(result1).barBaz == "bar2.0")
  }

  "cakeZ.bake with Generic" should "work correctly" in {
    val c2      = cakeZ[BarBazComponent with BarComponent with BazComponent]
    val result1 = c2.as[BarBazWiring] bake BarBazWiring(bar = "bar", baz = 2.0)

    assert(runtime.unsafeRun(result1).bar == "bar")
    assert(runtime.unsafeRun(result1).baz == 2.0)
  }

  "cakeZ.map" should "work correctly" in {
    val c1      = cakeZ[FooComponent].map(_.foo + 2)
    val result1 = c1.bake(1 :: HNil)
    assert(runtime.unsafeRun(result1) == 3)
  }

  "cakeZ.comap" should "work correctly" in {
    val c1      = cakeZ[FooComponent].comap[String](s => s.toInt :: HNil)
    val result1 = c1.bake("10")
    assert(runtime.unsafeRun(result1).foo == 10)
  }

  "cakeZ.flatMap" should "accumulate depedencies on the typelevel" in {
    val c1 = cakeZ[FooBarComponent with FooComponent with BarComponent]
    val c2 = cakeZ[BarBazComponent with BarComponent with BazComponent]

    val c3 = for {
      c1 <- c1
      c2 <- c2
    } yield {}

    assertCompiles("""implicitly[c3.Dependencies <:< (Int :: String :: Double :: HNil)]""")
  }

  it should "work correctly" in {
    val c1 = cakeZ[FooBarComponent with FooComponent with BarComponent]
    val c2 = cakeZ[BarBazComponent with BarComponent with BazComponent]

    val c3 = for {
      c1 <- c1
      c2 <- c2
    } yield (c1, c2)

    val (comp1, comp2) = runtime.unsafeRun(c3.bake(1 :: "foo" :: 2.0 :: HNil))

    assert(comp1.foo == 1)
    assert(comp1.bar == "foo")
    assert(comp2.bar == comp1.bar)
    assert(comp2.baz == 2.0)
  }

  "cakeZ" should "provide CakeZ with specific constructor" in {
    val c0 = cakeZ[ComponentWithConstructors with FooComponent]
    val c1 = cakeZ[ComponentWithConstructors with FooComponent](1)

    assertCompiles("""implicitly[c0.Dependencies <:< (BigInt :: Int :: HNil)]""")
    assertCompiles("""implicitly[c1.Dependencies <:< (Long :: Int :: HNil)]""")

    val program = for {
      c0 <- c0
      c1 <- c1
    } yield c0.bg + c1.bg

    assertCompiles("""implicitly[program.Dependencies <:< (BigInt :: Int :: Long :: HNil)]""")

    val result = program.bake(BigInt(1) :: 0 :: 2L :: HNil)
    assert(runtime.unsafeRun(result) == BigInt(3))
  }

  "CakeZ.succeed" should "work correctly" in {
    val c0 = CakeZ.succeed[Int](1)

    val result = c0.baked

    assert(runtime.unsafeRun(result) == 1)
  }

  "CakeZ.succeedLazy" should "work correctly" in {
    val c0 = CakeZ.succeedLazy[Int](1)

    val result = c0.baked

    assert(runtime.unsafeRun(result) == 1)
  }

  "CakeZ.fail" should "work correctly" in {
    val c0 = CakeZ.fail(1)

    val result = c0.baked.either

    assert(runtime.unsafeRun(result) == Left(1))
  }

  "CakeZ.liftF" should "work correctly" in {
    val c0 = CakeZ.liftF[Any, Throwable, Int](ZIO { 1 + 1 })

    val result = runtime unsafeRun c0.baked

    assert(result == 2)
  }

  "CakeZ.id" should "work correctly" in {
    val c0 = CakeZ.id[String]

    val result = runtime unsafeRun c0.bake("foo")

    assert(result == "foo")
  }

  "CakeZ.depMap" should "work correctly" in {
    val c0: CakeZ.Aux[Any, Throwable, FooComponent, FooWiringConcrete] =
      cakeZ[FooComponent].comap[FooWiringConcrete](_.foo :: HNil)
    val c1 = c0.depMap { (dep, fooComponent) =>
      s"${dep.foo}=${fooComponent.foo}"
    }

    val result = runtime unsafeRun c1.bake(FooWiringConcrete(1.tagged[foo]))

    assert(result == "1=1")
  }

  "CakeZ.depFlatMap" should "work correctly" in {
    val c0 = cakeZ[FooComponent]
    val c1 = c0.depFlatMap { (dep, fooComponent) =>
      cakeZ[BarComponent].map { bar =>
        s"$dep --> ${bar.bar}"
      }
    }

    assertCompiles("""implicitly[c1.Dependencies <:< (Int :: String :: HNil)]""")

    val result = runtime unsafeRun c1.bake(1 :: "foo" :: HNil)

    assert(result == "1 :: HNil --> foo")
  }

  class NotCake(a: Int, b: String) {
    def test: String = s"$a + $b"
  }

  "CakeZ.apF" should "work correctly" in {
    val c0 = CakeZ.apF[Any, Throwable, NotCake, Int :: String :: HNil] {
      case a :: b :: HNil =>
        ZIO(new NotCake(a, b))
    }

    val result = runtime unsafeRun c0.bake(1 :: "foo" :: HNil)
    assert(result.test == "1 + foo")
  }

  "CakeZ.ap" should "work correctly" in {
    val c0 = CakeZ.ap[NotCake, Int :: String :: HNil] {
      case a :: b :: HNil =>
        new NotCake(a, b)
    }

    val result = runtime unsafeRun c0.bake(1 :: "foo" :: HNil)
    assert(result.test == "1 + foo")
  }

  "CakeZ.comapM" should "work correctly" in {
    val c0: CakeZ.Aux[Any, Throwable, FooComponent, Int] = cakeZ[FooComponent]
    val c1 = c0.comapM { str: String =>
      ZIO(str.toInt)
    }

    val result = runtime.unsafeRun(c1 bake "2")
    assert(result.foo == 2)
  }
}
