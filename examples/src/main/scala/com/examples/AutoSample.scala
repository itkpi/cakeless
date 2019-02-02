package com.examples

import cakeless._
import cakeless.tagging._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object AutoSample {
  def main(args: Array[String]): Unit = {
    val props: Map[String, String] @@ props = Map("a" -> "b").tagged[props]
    val c                                   = cake[AllComponents2 with ExecutionContextComponent with PropsComponent]
    val wired                               = c.auto
    println(wired)
    println(wired.props)
    println(wired.ec)
    val future = wired.getPropAsync("a")
    println(future)
    println(Await.result(future, Duration.Inf))
  }
}
