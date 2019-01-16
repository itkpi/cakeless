package cakeless.internal

import shapeless._
import shapeless.ops.hlist._

/**
  * Reverse operation for [[Union]].
  * Allows to Undo union operation for hlist and case-classes
  *
  * @tparam A - some
  * @tparam B - some other
  * @tparam C - A ∪ B
  * */
trait UnUnion[A, B, C] {

  /**
    * Allows to undo [[Union]] operation for [[A]] and [[B]]
    *
    * @param c - union result
    * @return - undone operation
    * */
  def apply(c: C): (A, B)
}

object UnUnion {
  def apply[A, B, C](implicit ev: UnUnion[A, B, C]): UnUnion[A, B, C] = ev

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

  implicit def caseClassUnUnion[A, R1 <: HList, B, R2 <: HList, Out <: HList, OutR](
      implicit genA: Generic.Aux[A, R1],
      genB: Generic.Aux[B, R2],
      unUnion: UnUnion[R1, R2, Out],
      genOut: Generic.Aux[OutR, Out]
  ): UnUnion[A, B, OutR] = new UnUnion[A, B, OutR] {
    def apply(c: OutR): (A, B) = {
      val out      = genOut.to(c)
      val (r1, r2) = unUnion(out)
      (genA.from(r1), genB.from(r2))
    }
  }
}
