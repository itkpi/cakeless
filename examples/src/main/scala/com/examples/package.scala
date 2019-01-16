package com

import shapeless.Witness
import shapeless.syntax.singleton._

package object examples {
  type config = Witness.`"config"`.T
  type props  = Witness.`"props"`.T
  type token  = Witness.`"token"`.T
}
