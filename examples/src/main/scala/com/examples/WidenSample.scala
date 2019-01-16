package com.examples

import cakeless._
import cakeless.tagging._
import shapeless.{::, HNil, Witness}

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

  val comp1TestDropsHNil: Cake.Aux[Comp1, String] = cake[Comp1]
  println(comp1TestDropsHNil.bake("without HLIST").x)

  val baseCake: Cake.Aux[Comp2 with Comp1, Base]          = cake[Comp2 with Comp1]
  val sample: Cake.Aux[Comp2 with Comp1, Concrete]        = baseCake.widen
  val sample2: Cake.Aux[Comp2 with Comp1, ConcreteWiring] = baseCake.asR[ConcreteWiring].widen

  println(baseCake.bake(1 :: "BASE CAKE" :: HNil).xy)
  println(sample.bake(1.tagged[y] :: "WIDEN CAKE".tagged[x] :: HNil).xy)
  println(sample2.bake(ConcreteWiring(1.tagged[y], "WIDEN CASE CLASS CAKE".tagged[x])).xy)
}
