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

  val baseCake: Cake.Aux[Comp2 with Comp1, Base]   = cake[Comp2 with Comp1]
  val sample: Cake.Aux[Comp2 with Comp1, Concrete] = baseCake.widen

  println(baseCake.bake(1 :: "foo" :: HNil).xy)
  println(sample.bake(1.tagged[y] :: "foo".tagged[x] :: HNil).xy)
}
