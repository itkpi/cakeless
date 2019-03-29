package com.examples

import cakeless._
import cakeless.tagging._
import shapeless.{::, HNil, Witness}
import scalaz.zio._
import scalaz.zio.console._

object WidenSample extends App {
  type x = Witness.`"x"`.T
  type y = Witness.`"y"`.T

  trait Comp1 {
    def x: String
  }

  trait Comp2 { self: Comp1 =>
    def y: Int
    def xy: String = x + y
  }

  type Base     = Int :: String :: HNil
  type Concrete = (Int @@ y) :: (String @@ x) :: HNil

  case class ConcreteWiring(y: Int @@ y, x: String @@ x)

  def run(args: List[String]): ZIO[WidenSample.Environment, Nothing, Int] = {
    val comp1TestDropsHNil: CakeZ.Aux[Any, Nothing, Comp1, String] = cakeZ[Any, Nothing, Comp1]

    val baseCake: CakeZ.Aux[Any, Nothing, Comp2 with Comp1, Base]   = cakeZ[Any, Nothing, Comp2 with Comp1]
    val sample: CakeZ.Aux[Any, Nothing, Comp2 with Comp1, Concrete] = baseCake.widen

    for {
      comp1 <- comp1TestDropsHNil.bake("without HLIST")
      _     <- putStrLn(comp1.x)
      comp2 <- baseCake.bake(1 :: "BASE CAKE" :: HNil)
      _     <- putStrLn(comp2.xy)
      comp3 <- sample.bake(1.tagged[y] :: "WIDEN CAKE".tagged[x] :: HNil)
      _     <- putStrLn(comp3.xy)
    } yield 0
  }
}
