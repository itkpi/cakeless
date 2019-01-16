package cakeless

import shapeless.syntax.SingletonOps
import shapeless.tag

package object tagging {
  type @@[A, T] = tag.@@[A, T]

  implicit class TypelevelTagging[A](private val self: A) extends AnyVal {
    def tagged[T]: A @@ T = tag[T][A](self)

    /** requires [[shapeless.syntax.singleton]] import */
    def taggedAs(s: SingletonOps): A @@ s.T = tag[s.T][A](self)
  }
}
