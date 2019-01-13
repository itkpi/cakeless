package cakeless

import cats.{~>, Applicative, Id}
import scala.language.higherKinds

trait LowPriorityCakes {
  implicit def cakeFromApplicative[F[_], A](implicit F: Applicative[F], cake: Cake[A]): CakeT.Aux[F, A, cake.Dependencies] =
    cake.mapK[F](λ[Id[?] ~> F[?]](F.pure(_)))
}
