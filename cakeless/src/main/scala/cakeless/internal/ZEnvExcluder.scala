package cakeless.internal

import zio.NeedsEnv

trait ZEnvExcluder[R] {
  type Excluded

  def construct(r: Excluded): R
}


trait LowPriorityExcluder0 {
//  implicit def excludeAll[T]: ZEnvExcluder.Aux[T, Any] = ZEnvExcluder.create[T, Any]()
}

object ZEnvExcluder extends LowPriorityExcluder0 {
  type Aux[-R, Excluded0] = ZEnvExcluder[R] {type Excluded = Excluded0}

  protected[internal] def create[T, R](f: R => T) : Aux[T, R] = new ZEnvExcluder[T] {
    type Excluded = R
    override def construct(r: R): T = f(r)
  }

//  implicit def excludeRight[T, R](implicit ev0: T <:< R, ev1: NeedsEnv[R]): ZEnvExcluder.Aux[T, R] = ZEnvExcluder.create(t => ???)
}
