package cakeless.ioc.builder

import zio._
import scala.annotation.unchecked.uncheckedVariance

sealed trait ZUsage[-R, +E, +A] {
  type Self[-r, +e, +a] <: ZUsage[r, e, a]
  final type ZAux[-r, +e, +a] = ZUsage.Aux[r, e, a, Self]

  def use[R0 <: R, E1 >: E, A1 >: A](args: List[String])(managed: ZManaged[R0, E1, A1]): ZIO[R0, E1, A1]

  @`inline` def widenR[R0](implicit ev: R0 <| R): ZAux[R0, E, A]                         = this.asInstanceOf[ZAux[R0, E, A]]
  @`inline` def widenAllEv[R0, E1 >: E, A1 >: A](implicit ev: R0 <| R): ZAux[R0, E1, A1] = this.asInstanceOf[ZAux[R0, E1, A1]]
  @`inline` def widenAllSubt[R0 <: R, E1 >: E, A1 >: A]: ZAux[R0, E1, A1]                = this.asInstanceOf[ZAux[R0, E1, A1]]
}

object ZUsage {
  type Aux[-R, +E, +A, +Self0[-r, +e, +a] <: ZUsage[r, e, a]] = ZUsage[R, E, A] {
    type Self[-r, +e, +a] = Self0[r, e, a] @uncheckedVariance
  }

  sealed trait Base[-R, +E, +A] extends ZUsage[R, E, A]

  sealed trait NoUsage[-R, +E, +A] extends Base[R, E, A] {
    final override type Self[-r, +e, +a] = NoUsage[r, e, a]
    final override def use[R0 <: R, E1 >: E, A1 >: A](args: List[String])(managed: ZManaged[R0, E1, A1]): ZIO[R0, E1, A1] =
      managed.use(ZIO.succeed)
  }

  def noUsage[R, E, A]: Aux[R, E, A, NoUsage] = new NoUsage[R, E, A] {}

  sealed trait Using[-R, +E, +A] extends ZUsage[R, E, A] with Base[R, E, A] {
    final override type Self[-r, +e, +a] = Using[r, e, a]
  }

  def using[R, E, A](f: (List[String], ZManaged[R, E, A]) => ZIO[R, E, A]): Aux[R, E, A, Using] = new Using[R, E, A] {
    final override def use[R0 <: R, E1 >: E, A1 >: A](args: List[String])(managed: ZManaged[R0, E1, A1]): ZIO[R0, E1, A1] =
      f(args, managed.asInstanceOf[ZManaged[R, E, A]])
  }
}
