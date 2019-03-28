package cakeless.lifecycle

import cakeless.internal.CakeTBase

import scala.annotation.implicitNotFound
import scala.language.higherKinds

/**
  * Typeclass allowing to control cake component lifecycle withing context [[F]]
  *
  * @tparam F - monadic context
  * */
@implicitNotFound("""Cannot manage lifecycle for cakes in context ${F}""")
trait Lifecycle[F[_], CakeImpl[Fx[_], Ax] <: CakeTBase[Fx, Ax]] {

  type CakeImplAux[Fx[_], Ax, Deps] <: CakeImpl[Fx, Ax] { type Dependencies = Deps }

  /**
    * Allows to execute monadic side-effect
    * exactly before [[A]] is instantiated
    *
    * @tparam A - component type
    * @param cake - cake
    * @param thunk - monadic side-effect
    * @return - same cake for which side-effect will be executed before [[A]] is instantiated
    * */
  def preStartF[A](cake: CakeImpl[F, A])(thunk: => F[Unit]): CakeImplAux[F, A, cake.Dependencies]

  /**
    * Allows to execute side-effect
    * exactly before [[A]] is instantiated
    *
    * @tparam A - component type
    * @param cake - cake
    * @param thunk - side-effect
    * @return - same cake for which side-effect will be executed before [[A]] is instantiated
    * */
  def preStart[A](cake: CakeImpl[F, A])(thunk: => Unit): CakeImplAux[F, A, cake.Dependencies]

  /**
    * Allows to execute monadic side-effect
    * after [[A]] is successfully instantiated
    *
    * @tparam A - component type
    * @param cake - cake
    * @param thunk - monadic side-effect
    * @return - same cake for which side-effect will be executed after [[A]] is successfully instantiated
    * */
  def postStartF[A](cake: CakeImpl[F, A])(thunk: => F[Unit]): CakeImplAux[F, A, cake.Dependencies]

  /**
    * Allows to execute side-effect
    * after [[A]] is successfully instantiated
    *
    * @tparam A - component type
    * @param cake - cake
    * @param thunk - monadic side-effect
    * @return - same cake for which side-effect will be executed after [[A]] is successfully instantiated
    * */
  def postStart[A](cake: CakeImpl[F, A])(thunk: => Unit): CakeImplAux[F, A, cake.Dependencies]

  /**
    * Allows to execute monadic side-effect
    * depending on [[A]] component
    * after [[A]] is successfully instantiated.
    *
    * Useful for acquiring resources before program is started
    * (for instance opening connection with Database
    * or starting cache).
    *
    * @tparam A - component type
    * @param cake - cake
    * @param use - component-dependent monadic side-effect
    * @return - same cake for which side-effect will be executed after [[A]] is successfully instantiated
    * */
  def postStartUseF[A](cake: CakeImpl[F, A])(use: A => F[Unit]): CakeImplAux[F, A, cake.Dependencies]

  /**
    * Allows to execute side-effect
    * depending on [[A]] component
    * after [[A]] is successfully instantiated.
    *
    * Useful for logging or other pure operations.
    *
    * @tparam A - component type
    * @param cake - cake
    * @param use - component-dependent side-effect
    * @return - same cake for which side-effect will be executed after [[A]] is successfully instantiated
    * */
  def postStartUse[A](cake: CakeImpl[F, A])(use: A => Unit): CakeImplAux[F, A, cake.Dependencies]

  /**
    * Allows to handle instantiation errors
    * providing new [[A]] component in context [[F]]
    * dependent on specific error.
    *
    * @tparam A - component type
    * @param cake - cake
    * @param f - error-handling function
    * @return - same cake if no errors occurred or new component based on specific error
    * */
  def handleErrorWith[A](cake: CakeImpl[F, A])(f: Throwable => F[A]): CakeImplAux[F, A, cake.Dependencies]

  /**
    * Allows to handle instantiation errors
    * providing new [[A]] component
    * dependent on specific error.
    *
    * @tparam A - component type
    * @param cake - cake
    * @param f - error-handling function
    * @return - same cake if no errors occurred or new component based on specific error
    * */
  def handleError[A](cake: CakeImpl[F, A])(f: Throwable => A): CakeImplAux[F, A, cake.Dependencies]

  /**
    * Allows to handle instantiation errors
    * providing new [[A]] component in context [[F]]
    * if provided partial function is defined on occurred error
    *
    * @tparam A - component type
    * @param cake - cake
    * @param pf - error-handling function
    * @return - same cake if no errors occurred or new component based on thrown error if PartialFunction is defined on that error.
    * */
  def recoverWith[A](
      cake: CakeImpl[F, A]
  )(pf: PartialFunction[Throwable, F[A]]): CakeImplAux[F, A, cake.Dependencies] =
    handleErrorWith(cake)(pf.applyOrElse(_, default = (e: Throwable) => throw e))

  /**
    * Allows to handle instantiation errors
    * providing new [[A]] component
    * if provided partial function is defined on occurred error
    *
    * @tparam A - component type
    * @param cake - cake
    * @param pf - error-handling function
    * @return - same cake if no errors occurred or new component based on thrown error if PartialFunction is defined on that error.
    * */
  def recover[A](cake: CakeImpl[F, A])(pf: PartialFunction[Throwable, A]): CakeImplAux[F, A, cake.Dependencies]
}
