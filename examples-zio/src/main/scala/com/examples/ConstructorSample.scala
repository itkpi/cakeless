package com.examples

import java.nio.file.{Path, Paths}

import cakeless._
import scalaz.zio._
import scalaz.zio.console._
import cakeless.tagging._
import shapeless.{::, HNil}
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global

object ConstructorSample extends App {
  def run(args: List[String]): ZIO[LifecycleExample.Environment, Nothing, Int] = {

    type Component = NestedComponent with AllComponents2 with ExecutionContextComponent with PropsComponent

    val cake1: CakeZ.Aux[
      Any,
      Throwable,
      Component,
      (String @@ token) :: ExecutionContext :: (Map[String, String] @@ props) :: HNil
    ] =
      cakeZ[Component]

    val cake2: CakeZ.Aux[
      Any,
      Throwable,
      Component,
      (String @@ username) :: (String @@ password) :: ExecutionContext :: (Map[String, String] @@ props) :: HNil
    ] =
      cakeZ[Component](1)

    val cake3: CakeZ.Aux[
      Any,
      Throwable,
      Component,
      Int :: ExecutionContext :: (Map[String, String] @@ props) :: HNil
    ] =
      cakeZ[Component](2)

    ???
  }

}
