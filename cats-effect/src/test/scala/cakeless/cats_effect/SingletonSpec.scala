package cakeless.cats_effect

import cakeless._
import cats.effect.IO
import org.scalatest.FlatSpec
import shapeless.{::, HNil}

class SingletonSpec extends FlatSpec {
  "CakeT.singleton" should "work correctly" in {
    var i: Int = 0
    trait SampleComponent {
      def foo: Int
      i += 1
    }
    val singletonCake = cakeSingleton[IO, SampleComponent]
    val result        = singletonCake.bake(1 :: HNil).unsafeRunSync()

    assert(result.foo == 1)
    assert(i == 1)

    singletonCake.bake(1 :: HNil).unsafeRunSync() // call again

    assert(i == 1)
  }
}
