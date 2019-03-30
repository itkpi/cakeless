package cakeless

import scalaz.zio._
import org.scalatest.FlatSpec
import shapeless.HNil

class SingletonSpec extends FlatSpec {
  private val runtime = new DefaultRuntime {}

  "cakeSingleton" should "work correctly" in {
    var i: Int = 0
    trait SampleComponent {
      def foo: Int
      i += 1
    }
    val singletonCake = cakeSingleton[SampleComponent]

    val program = for {
      _ <- singletonCake.bake(1 :: HNil)
      _ <- singletonCake.bake(1 :: HNil)
    } yield ()

    runtime unsafeRun program

    assert(i == 1)
  }
}
