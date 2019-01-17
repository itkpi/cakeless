package cakeless.lifecycle

import cakeless.CakeT
import cakeless.CakeT.Aux
import cats.Applicative
import cats.effect.ExitCase
import scala.language.higherKinds

/**
  * Copy-pasted definition from [[cats.effect.Bracket]]
  * */
trait Bracket[F[_], E] {

  /**
    * A generalized version of [[bracket]] which uses [[ExitCase]]
    * to distinguish between different exit cases when releasing
    * the acquired resource.
    *
    * @param acquire $acquireParam
    * @param use $useParam
    * @param release is the action that's supposed to release the
    *        allocated resource after `use` is done, by observing
    *        and acting on its exit condition
    */
  def bracketCase[A, B](acquire: CakeT[F, A])(use: A => F[B])(release: (A, ExitCase[E]) => F[Unit]): CakeT.Aux[F, B, acquire.Dependencies]

  /**
    * Operation meant for specifying tasks with safe resource
    * acquisition and release in the face of errors and interruption.
    *
    * This operation provides the equivalent of `try/catch/finally`
    * statements in mainstream imperative languages for resource
    * acquisition and release.
    *
    * @param acquire $acquireParam
    * @param use $useParam
    * @param release is the action that's supposed to release the
    *        allocated resource after `use` is done, irregardless of
    *        its exit condition
    */
  def bracket[A, B](acquire: CakeT[F, A])(use: A => F[B])(release: A => F[Unit]): CakeT.Aux[F, B, acquire.Dependencies] =
    bracketCase(acquire)(use)((a, _) => release(a))

  /**
    * Operation meant for ensuring a given task continues execution even
    * when interrupted.
    */
  def uncancelable[A](fa: CakeT[F, A])(implicit F: Applicative[F]): CakeT.Aux[F, A, fa.Dependencies] =
    bracket(fa)(F.pure)(_ => F.unit)

  /**
    * Executes the given `finalizer` when the source is finished,
    * either in success or in error, or if canceled.
    *
    * This variant of [[guaranteeCase]] evaluates the given `finalizer`
    * regardless of how the source gets terminated:
    *
    *  - normal completion
    *  - completion in error
    *  - cancelation
    *
    * As best practice, it's not a good idea to release resources
    * via `guaranteeCase` in polymorphic code. Prefer [[bracket]]
    * for the acquisition and release of resources.
    *
    * @see [[guaranteeCase]] for the version that can discriminate
    *      between termination conditions
    * @see [[bracket]] for the more general operation
    */
  def guarantee[A](fa: CakeT[F, A])(finalizer: F[Unit])(implicit F: Applicative[F]): CakeT.Aux[F, A, fa.Dependencies] =
    bracket(fa)(F.pure)(_ => finalizer)

  /**
    * Executes the given `finalizer` when the source is finished,
    * either in success or in error, or if canceled, allowing
    * for differentiating between exit conditions.
    *
    * This variant of [[guarantee]] injects an [[ExitCase]] in
    * the provided function, allowing one to make a difference
    * between:
    *
    *  - normal completion
    *  - completion in error
    *  - cancelation
    *
    * As best practice, it's not a good idea to release resources
    * via `guaranteeCase` in polymorphic code. Prefer [[bracketCase]]
    * for the acquisition and release of resources.
    *
    * @see [[guarantee]] for the simpler version
    * @see [[bracketCase]] for the more general operation
    */
  def guaranteeCase[A](fa: CakeT[F, A])(finalizer: ExitCase[E] => F[Unit])(implicit F: Applicative[F]): CakeT.Aux[F, A, fa.Dependencies] =
    bracketCase(fa)(F.pure)((_, e) => finalizer(e))
}

trait LowPriorityBracket {
  import cats.effect.{Bracket => CEBracket}

  implicit def fromCatsEffect[F[_], E](implicit F: CEBracket[F, E]): Bracket[F, E] = new Bracket[F, E] {
    def bracketCase[A, B](acquire: CakeT[F, A])(use: A => F[B])(
        release: (A, ExitCase[E]) => F[Unit]
    ): Aux[F, B, acquire.Dependencies] = new CakeT[F, B] {
      type Dependencies = acquire.Dependencies
      def bake(deps: acquire.Dependencies): F[B] =
        F.bracketCase[A, B](acquire = acquire bake deps)(use)(release)

    }
  }
}

object Bracket extends LowPriorityBracket {
  def apply[F[_], E](implicit ev: Bracket[F, E]): Bracket[F, E] = ev
}
