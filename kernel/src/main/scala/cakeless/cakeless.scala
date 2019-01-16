import scala.language.experimental.macros
import scala.language.{higherKinds, implicitConversions}
import cats.Id
import cakeless.internal.DependencyResolver

package object cakeless {
  type Cake[A] = CakeT[Id, A]

  object Cake {
    type Aux[A, D0] = Cake[A] {type Dependencies = D0}
  }

  def cake[A]: Cake[A] = macro DependencyResolver.makeCake0[A]

  def cake[A](constructor: Int): Cake[A] = macro DependencyResolver.makeCake[A]

  def cakeT[F[_], A]: CakeT[F, A] = macro DependencyResolver.makeCakeT0[F, A]

  def cakeT[F[_], A](constructor: Int): CakeT[F, A] = macro DependencyResolver.makeCakeT[F, A]
}
