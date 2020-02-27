package cakeless.ioc

package object builder {
  type <|[-A, +B] = Liskov[A, B]
  type |>[+B, -A] = Liskov[A, B]
  type ⋃[+A, +B]  = A with B
}
