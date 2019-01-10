package com.github.cakeless.internal

import shapeless._
import shapeless.ops.hlist._

/**
  * Reverse operation for [[Union]]
  *
  * @tparam A - some hlist
  * @tparam B - some other hlist
  * @tparam C - A ∪ B
  * */
trait UnUnion[A <: HList, B <: HList, C <: HList] {

  /**
    * Allows to undo [[Union]] operation for [[A]] and [[B]]
    *
    * @param c - union result
    * @return - undone operation
    * */
  def apply(c: C): (A, B)
}

object UnUnion {
  implicit def hlistUnUnion[M <: HList]: UnUnion[HNil, M, M] = new UnUnion[HNil, M, M] {
    def apply(c: M): (HNil, M) = (HNil, c)
  }

  implicit def hlistUnUnion1[H, T <: HList, M <: HList, U <: HList](
      implicit u: UnUnion[T, M, U],
      f: FilterNot.Aux[M, H, M]
  ): UnUnion[H :: T, M, H :: U] =
    new UnUnion[H :: T, M, H :: U] {
      def apply(c: H :: U): (H :: T, M) = {
        val (t, m) = u(c.tail)
        (c.head :: t, m)
      }
    }

  implicit def hlistUnUnion2[H, T <: HList, M <: HList, MR <: HList, U <: HList](
      implicit r: Remove.Aux[M, H, (H, MR)],
      u: UnUnion[T, MR, U]
  ): UnUnion[H :: T, M, H :: U] =
    new UnUnion[H :: T, M, H :: U] {
      def apply(c: H :: U): (H :: T, M) = {
        val (t, mr) = u(c.tail)
        (c.head :: t, r.reinsert((c.head, mr)))
      }
    }
}
