package cakeless

/** Nat instances (0 to 10) */
package object nat {
  type _0  = Nat.Zero
  type _1  = Nat.Inc[_0]
  type _2  = Nat.Inc[_1]
  type _3  = Nat.Inc[_2]
  type _4  = Nat.Inc[_3]
  type _5  = Nat.Inc[_4]
  type _6  = Nat.Inc[_5]
  type _7  = Nat.Inc[_6]
  type _8  = Nat.Inc[_7]
  type _9  = Nat.Inc[_8]
  type _10 = Nat.Inc[_9]
}
