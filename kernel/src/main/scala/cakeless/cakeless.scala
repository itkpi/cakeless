import scala.language.experimental.macros
import scala.language.{higherKinds, implicitConversions}
import cats.Id
import cakeless.internal.{DependencyResolver, SyncResolver}
import shapeless.{::, HNil}

package object cakeless {

  /** alias for [[CakeT]] with [[Id]] context */
  type Cake[A] = CakeT[Id, A]

  object Cake {

    /** alias for [[CakeT.Aux]] with [[Id]] context */
    type Aux[A, D0] = Cake[A] { type Dependencies = D0 }

    /**
      * @see [[CakeT.pure]]
      * */
    def pure[A](a: A): Cake.Aux[A, HNil] =
      new CakeT[Id, A] {
        type Dependencies = HNil

        def bake(deps: HNil): A = a
      }

    /**
      * @see [[CakeT.id]]
      * */
    def id[D0]: Cake.Aux[D0, D0] = CakeT.id[Id, D0]

    /**
      * @see [[CakeT.ap]]
      * */
    def ap[A, D0](f: D0 => A): Cake.Aux[A, D0] = CakeT.ap[Id, A, D0](f)
  }

  /**
    * Entry point for component wiring.
    * Allows to wrap [[A]] into [[Cake]].
    *
    * [[A]] should be:
    * - abstract class or trait with abstract `def`s or `val`s
    * - or abstract class with both constructor parameters and abstract `def`s or `val`s
    * [[A]] is allowed to have self-type requirements.
    * Dependencies of [[A]] self-types will be also picked-up
    * */
  def cake[A]: Cake[A] = macro DependencyResolver.makeCake0[A]

  /**
    * Entry point for component wiring.
    * Allows to wrap [[A]] into [[Cake]].
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
  def cake[A](constructor: Int): Cake[A] = macro DependencyResolver.makeCake[A]

  /**
    * Entry point for component wiring.
    * Allows to wrap [[A]] into [[CakeT]] in context [[F]].
    * Requires [[cats.Applicative]] for context [[F]]
    *
    * [[A]] should be:
    * - abstract class or trait with abstract `def`s or `val`s
    * - or abstract class with both constructor parameters and abstract `def`s or `val`s
    * [[A]] is allowed to have self-type requirements.
    * Dependencies of [[A]] self-types will be also picked-up
    * */
  def cakeT[F[_], A]: CakeT[F, A] = macro DependencyResolver.makeCakeT0[F, A]

  /**
    * Entry point for component wiring.
    * Allows to wrap [[A]] into [[CakeT]] in context [[F]].
    * Requires [[cats.Applicative]] for context [[F]]
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
  def cakeT[F[_], A](constructor: Int): CakeT[F, A] = macro DependencyResolver.makeCakeT[F, A]

  /**
    * Entry point for component wiring.
    * Allows to wrap [[A]] into [[CakeT]]
    * delaying [[A]] instantiation using context [[F]]
    * Requires [[cats.effect.Sync]] for context [[F]]
    *
    * [[A]] should be:
    * - abstract class with both constructor parameters and abstract `def`s or `val`s
    * [[A]] is allowed to have self-type requirements.
    * Dependencies of [[A]] self-types will be also picked-up
    *
    * */
  def cakeDelayed[F[_], A]: CakeT[F, A] = macro SyncResolver.makeCakeSync0[F, A]

  /**
    * Entry point for component wiring.
    * Allows to wrap [[A]] into [[CakeT]]
    * delaying [[A]] instantiation using context [[F]]
    * Requires [[cats.effect.Sync]] for context [[F]]
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
  def cakeDelayed[F[_], A](constructor: Int): CakeT[F, A] = macro SyncResolver.makeCakeSync[F, A]

  /**
    * Entry point for component wiring.
    * Allows to wrap [[A]] into [[CakeT]]
    * delaying [[A]] instantiation using context [[F]]
    * Requires [[cats.effect.Sync]] for context [[F]].
    *
    * Additionally ensures that [[A]] will be singleton component
    * (which means [[A]] will be allocated exactly once
    * no matter how much cake depends on it)
    * Implemented using [[cats.effect.concurrent.Ref]]
    *
    * [[A]] should be:
    * - abstract class with both constructor parameters and abstract `def`s or `val`s
    * [[A]] is allowed to have self-type requirements.
    * Dependencies of [[A]] self-types will be also picked-up
    *
    * */
  def cakeSingleton[F[_], A]: CakeT[F, A] = macro SyncResolver.makeCakeSyncSingleton0[F, A]

  /**
    * Entry point for component wiring.
    * Allows to wrap [[A]] into [[CakeT]]
    * delaying [[A]] instantiation using context [[F]]
    * Requires [[cats.effect.Sync]] for context [[F]].
    *
    * Additionally ensures that [[A]] will be singleton component
    * (which means [[A]] will be allocated exactly once
    * no matter how much cake depends on it)
    * Implemented using [[cats.effect.concurrent.Ref]]
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
  def cakeSingleton[F[_], A](constructor: Int): CakeT[F, A] = macro SyncResolver.makeCakeSyncSingleton[F, A]



  /**
    * Implicit conversion used to drop [[HNil]] from the dependency type
    * when cake's dependency type is [[shapeless.HList]] with single value.
    *  */
  implicit def dropHNilDependency[F[_], A, D](cake: CakeT.Aux[F, A, D :: HNil]): CakeT.Aux[F, A, D] =
    cake.comap[D](_ :: HNil)
}
