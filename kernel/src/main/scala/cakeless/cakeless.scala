import scala.language.experimental.macros
import scala.language.{higherKinds, implicitConversions}
import cats.Id
import cakeless.internal.DependencyResolver

package object cakeless extends LowPriorityCakes {
  type Cake[A] = CakeT[Id, A]
  object Cake {
    type Aux[A, D0] = Cake[A] { type Dependencies = D0 }
  }

  implicit def derive[A]: Cake[A] = macro DependencyResolver.makeCake[A]

  def cake[A](implicit ev: Cake[A]): Cake.Aux[A, ev.Dependencies]                = ev
  def cakeT[F[_], A](implicit ev: CakeT[F, A]): CakeT.Aux[F, A, ev.Dependencies] = ev
}
