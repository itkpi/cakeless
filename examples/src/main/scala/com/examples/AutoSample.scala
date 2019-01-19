package com.examples

import cakeless._

object AutoSample extends App {
  val c = cake[ExecutionContextComponent with PropsComponent]
  c.auto
}
