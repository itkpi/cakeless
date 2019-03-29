import cakeless.internal.{AutoBake, CakeZAutoBacking, ZioResolver}
import shapeless.{::, HNil}
import scala.language.experimental.macros
import scala.language.{higherKinds, implicitConversions}

package object cakeless {

  /**
    * Entry point for component wiring.
    * Allows to wrap [[A]] into [[CakeZ]]
    * delaying [[A]] instantiation using ZIO
    *
    * [[A]] should be:
    * - abstract class with both constructor parameters and abstract `def`s or `val`s
    * [[A]] is allowed to have self-type requirements.
    * Dependencies of [[A]] self-types will be also picked-up
    *
    * */
  def cakeZ[Effect, Error, A]: CakeZ[Effect, Error, A] = macro ZioResolver.makeZSync0[Effect, Error, A]

  /**
    * Entry point for component wiring.
    * Allows to wrap [[A]] into [[CakeZ]]
    * delaying [[A]] instantiation using ZIO
    *
    * `constructor` parameter allows to chose appropriate constructor.
    * If constructor is `0` - primary constructor will be used.
    * If constructor is `> 0` - constructor will be picked by definition order
    *
    * `constructor` is required to be literal value.
    * Otherwise code won't compile!!!
    *
    * [[A]] should be:
    * - abstract class with both constructor parameters and abstract `def`s or `val`s
    * [[A]] is allowed to have self-type requirements.
    * Dependencies of [[A]] self-types will be also picked-up
    *
    * @param constructor - sequence number of constructor whose type signature should be used as dependency type for cake
    * */
  def cakeZ[Effect, Error, A](constructor: Int): CakeZ[Effect, Error, A] = macro ZioResolver.makeZSync[Effect, Error, A]

  /**
    * Entry point for component wiring.
    * Allows to wrap [[A]] into [[CakeZ]]
    * delaying [[A]] instantiation using ZIO
    *
    * Additionally ensures that [[A]] will be singleton component
    * (which means [[A]] will be allocated exactly once
    * no matter how much cake depends on it)
    * Implemented using [[scalaz.zio.Ref]]
    *
    * [[A]] should be:
    * - abstract class with both constructor parameters and abstract `def`s or `val`s
    * [[A]] is allowed to have self-type requirements.
    * Dependencies of [[A]] self-types will be also picked-up
    *
    * */
  def cakeSingleton[Effect, Error, A]: CakeZ[Effect, Error, A] = macro ZioResolver.makeCakeZSingleton0[Effect, Error, A]

  /**
    * Entry point for component wiring.
    * Allows to wrap [[A]] into [[CakeZ]]
    * delaying [[A]] instantiation using ZIO
    *
    * Additionally ensures that [[A]] will be singleton component
    * (which means [[A]] will be allocated exactly once
    * no matter how much cake depends on it)
    * Implemented using [[scalaz.zio.Ref]]
    *
    * `constructor` parameter allows to chose appropriate constructor.
    * If constructor is `0` - primary constructor will be used.
    * If constructor is `> 0` - constructor will be picked by definition order
    *
    * `constructor` is required to be literal value.
    * Otherwise code won't compile!!!
    *
    * [[A]] should be:
    * - abstract class with both constructor parameters and abstract `def`s or `val`s
    * [[A]] is allowed to have self-type requirements.
    * Dependencies of [[A]] self-types will be also picked-up
    *
    * */
  def cakeSingleton[Effect, Error, A](constructor: Int): CakeZ[Effect, Error, A] = macro ZioResolver.makeCakeZSingleton[Effect, Error, A]

  /**
    * Implicit conversion used to drop [[HNil]] from the dependency type
    * when cake's dependency type is [[shapeless.HList]] with single value.
    *  */
  implicit def dropHNilDependency[Effect, Error, A, D](cake: CakeZ.Aux[Effect, Error, A, D :: HNil]): CakeZ.Aux[Effect, Error, A, D] =
    cake.comap[D](_ :: HNil)

  implicit def materialize[F[_], A, Deps]: AutoBake[F, A, Deps] = macro CakeZAutoBacking.autoBakeImpl[F, A, Deps]
}
