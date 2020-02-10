package cakeless

import zio._

/**
  * Provides simple component lifecycle management.
  * Defines what kind of computations will be executed before the component instantiation
  * and after the component was successfully initialized.
  * */
class Lifecycle[-R1, -R2, -A] private (val preStartURIO: URIO[R1, Unit], val postStartURIO: A => URIO[R2, Unit]) { self =>

  /**
    * @param thunk - computation to be executed before [[A]] got initialized
    * */
  def preStart[R3 <: R1](thunk: URIO[R3, Unit]): Lifecycle[R3, R2, A] =
    new Lifecycle[R3, R2, A](preStartURIO *> thunk, postStartURIO)

  /**
    * @param thunk - computation to be executed after [[A]] got initialized
    * */
  def postStart[R3 <: R2](thunk: URIO[R3, Unit]): Lifecycle[R1, R3, A] =
    new Lifecycle[R1, R3, A](preStartURIO, postStartURIO(_) *> thunk)

  /**
    * @param use - computation depending on recently initialized [[A]]
    * */
  def postStart[R3 <: R2, AA <: A](use: AA => URIO[R3, Unit]): Lifecycle[R1, R3, AA] =
    new Lifecycle[R1, R3, AA](
      preStartURIO,
      a => postStartURIO(a) *> use(a)
    )

  /**
    * Allows to chain lifecycles using [[ZIO]] composition.
    *
    * @param that - some other lifecycle
   **/
  def &&[R3 <: R1, R4 <: R2, AA <: A](that: Lifecycle[R3, R4, AA]): Lifecycle[R3, R4, AA] =
    new Lifecycle[R3, R4, AA](
      preStartURIO = self.preStartURIO *> that.preStartURIO,
      postStartURIO = aa => self.postStartURIO(aa) *> that.postStartURIO(aa)
    )
}

object Lifecycle {

  /** Instance of Empty lifecycle (no behavior) */
  val empty: Lifecycle[Any, Any, Any] = new Lifecycle(URIO.unit, (_: Any) => URIO.unit)

  /* Entry points for building lifecycle */
  def preStart[R1](thunk: URIO[R1, Unit]): Lifecycle[R1, Any, Any]      = empty.preStart(thunk)
  def postStart[R2](thunk: URIO[R2, Unit]): Lifecycle[Any, R2, Any]     = empty.postStart(thunk)
  def postStart[A, R2](use: A => URIO[R2, Unit]): Lifecycle[Any, R2, A] = empty.postStart(use)
}
