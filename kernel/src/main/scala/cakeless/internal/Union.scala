package cakeless.internal

import shapeless.{DepFn2, Generic, HList}
import shapeless.ops.hlist.{Union => SUnion}

trait Union[A, B] extends DepFn2[A, B] with Serializable
object Union {
  type Aux[A, B, Out0] = Union[A, B] { type Out = Out0 }

  implicit def fromShapeless[A <: HList, B <: HList](implicit ev: SUnion[A, B]): Union.Aux[A, B, ev.Out] =
    new Union[A, B] {
      type Out = ev.Out
      def apply(t: A, u: B): ev.Out = ev(t, u)
    }

  implicit def caseClassUnion[A, R1 <: HList, B, R2 <: HList, Out <: HList, OutR](
      implicit genA: Generic.Aux[A, R1],
      genB: Generic.Aux[B, R2],
      union: SUnion.Aux[R1, R2, Out],
      genOut: Generic.Aux[OutR, Out]
  ): Union.Aux[A, B, OutR] = new Union[A, B] {
    type Out = OutR

    def apply(t: A, u: B): OutR = {
      val r1  = genA.to(t)
      val r2  = genB.to(u)
      val out = union(r1, r2)
      genOut.from(out)
    }
  }
}
