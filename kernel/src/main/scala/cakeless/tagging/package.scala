package cakeless

import shapeless.syntax.SingletonOps
import shapeless.tag

package object tagging {

  /** alias for [[shapeless.tag.@@]] */
  type @@[A, T] = tag.@@[A, T]

  /** allows to tag pure values */
  implicit class TypelevelTagging[A](private val self: A) extends AnyVal {

    /**
      * Tags [[A]] value with [[T]] type.
      *
      * Best practice is to use [[shapeless.Witness]] for tagging
      * instead of creating traits which will never be instantiated.
      *
      * {{{
      *   type foo = Witness.`"foo"`.T
      *
      *   val a: Int @@ foo = 1.tagged[foo]
      * }}}
      *
      * @return - [[A]] tagged with [[T]]
      * */
    def tagged[T]: A @@ T = tag[T][A](self)

    /**
      * Other version of [[tagged]]
      * using singleton ops syntax provided by shapeless.
      *
      * Pros:
      * - no need to define type aliases using [[shapeless.Witness]]
      * - will work in scala 2.13 because of native literal-type support
      *
      * Cons:
      * - Not IDEA friendly (your editor will be totally red)
      *
      * requires [[shapeless.syntax.singleton]] import
      *
      * {{{
      *
      *   val a = 1 taggedAs "foo"
      *   // equivalent to
      *   val a = 1.tagged[Witness.`"foo"`.T]
      * }}}
      *
      * @param s - literal type
      * @return - [[A]] tagged with literal type
      * */
    def taggedAs(s: SingletonOps): A @@ s.T = tagged[s.T]
  }
}
