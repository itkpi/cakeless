package cakeless

import cakeless.internal.{UnUnion, Union}
import cats.data.{ReaderT, WriterT}
import cats.kernel.Monoid
import cats.{~>, Applicative, FlatMap, Functor, Monad}
import shapeless.{Generic, HList, Nat}
import scala.annotation.implicitNotFound
import scala.language.higherKinds

@implicitNotFound("""
    Unable to find CakeT for ${A} in context ${F}.
    Please ensure that you haven't messed up with self-typing rules
  """)
trait CakeT[F[_], A] extends Serializable { self =>
  type Dependencies

  def bake(deps: Dependencies): F[A]

  def as[R](implicit gen: Generic.Aux[R, Dependencies], F: Functor[F]): CakeT.Aux[F, A, R] =
    comap[R](gen.to)

  def comap[D2](f: D2 => Dependencies): CakeT.Aux[F, A, D2] =
    new CakeT[F, A] {
      type Dependencies = D2

      def bake(deps: D2): F[A] = self.bake(f(deps))
    }

  def map[B](f: A => B)(implicit F: Functor[F]): CakeT.Aux[F, B, Dependencies] =
    new CakeT[F, B] {
      type Dependencies = self.Dependencies

      def bake(deps: self.Dependencies): F[B] = F.map(self bake deps)(f)
    }

  def flatMap[B, D1, Out, AL <: Nat](
      f: A => CakeT.Aux[F, B, D1]
  )(implicit union: Union.Aux[self.Dependencies, D1, Out],
    unUnion: UnUnion[self.Dependencies, D1, Out],
    F: FlatMap[F]): CakeT.Aux[F, B, Out] =
    new CakeT[F, B] {
      type Dependencies = Out

      def bake(deps: Dependencies): F[B] = {
        val (deps0, deps1) = unUnion(deps)
        F.flatMap(self bake deps0) { a =>
          f(a) bake deps1
        }
      }
    }

  def mapM[B](f: A => F[B])(implicit F: Monad[F]): CakeT.Aux[F, B, Dependencies] =
    new CakeT[F, B] {
      type Dependencies = self.Dependencies

      def bake(deps: Dependencies): F[B] = F.flatMap(self bake deps)(f)
    }

  def mapK[G[_]](arrow: F ~> G): CakeT.Aux[G, A, self.Dependencies] = new CakeT[G, A] {
    type Dependencies = self.Dependencies

    def bake(deps: Dependencies): G[A] = arrow(self bake deps)
  }

  def toReader: ReaderT[F, Dependencies, A] = ReaderT[F, Dependencies, A](bake)

  def logged[L: Monoid](logRecord: L)(implicit F: Applicative[F]): CakeT.Aux[WriterT[F, L, ?], A, Dependencies] =
    new CakeT[WriterT[F, L, ?], A] {
      type Dependencies = self.Dependencies
      def bake(deps: Dependencies): WriterT[F, L, A] = WriterT.liftF(self bake deps).tell(logRecord)
    }

  def widen[D0 <: Dependencies]: CakeT.Aux[F, A, D0] =
    self.asInstanceOf[CakeT.Aux[F, A, D0]]
}

object CakeT {
  type Aux[F[_], A, D0] = CakeT[F, A] { type Dependencies = D0 }
}
