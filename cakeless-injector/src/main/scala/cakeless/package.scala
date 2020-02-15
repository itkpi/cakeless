import cakeless.kernel.Wiring

package object cakeless {
  type Id[+A] = A

  @`inline` def a[B]: Wiring[B]  = new Wiring[B]()
  @`inline` def an[A]: Wiring[A] = a[A]
}
