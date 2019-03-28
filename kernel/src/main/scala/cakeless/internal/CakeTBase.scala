package cakeless.internal

import shapeless.HNil
import scala.language.higherKinds

/**
  * CakeT is monad for cake-pattern.
  * It is better version of classic Reader monad.
  *
  * Classic Reader monad is functional Dependency Injection pattern
  * allowing to define computation depending on some values that will be provided later
  * (typically in `main` method).
  * But its `flatMap` method allows to pass only Reader with same dependency type.
  *
  * Unlike classic Reader, CakeT allows to accumulate dependencies in `flatMap` monad.
  * `flatMap` takes CakeT with different dependency type.
  * `flatMap` resulting type is `Union` type of both `self.Dependency` and `that.Dependency`
  * - if `self` and `that` have same dependencies - CakeT will behave like classic Reader
  * - if `self` and `that` have different dependencies - resulting CakeT will have dependency type
  *   which is (`self.Dependency` ∪ `that.Dependency`)
  *
  * Dependencies are picked up by special macro on the type level.
  * By default they are represented as `shapeless.HList`.
  * User is allowed to transform the dependency using `comap` (typical Co-functor operation).
  * User is also allowed to transform the dependency into `case class` using `shapeless.Generic`
  *
  * @see https://medium.com/@itseranga/scala-cake-pattern-e0cd894dae4e for cake-pattern example
  * @see http://eed3si9n.com/herding-cats/Reader.html for reader monad example
  * @tparam F - Cake wiring context
  * @tparam A - cake component type
  * */
trait CakeTBase[F[_], A] extends Serializable { self =>

  /**
    * CakeT dependent type which is picked on the type level by whitebox macro.
    * Typically is `shapeless.HList`
    * */
  type Dependencies

  /**
    * `bake` method is used to provide the dependencies and instantiate [[A]] component within [[F]] context
    *
    * @param deps - dependencies
    * @return - wired component in [[F]] context
    * */
  def bake(deps: Dependencies): F[A]

  /**
    * [[bake]] for cases when [[Dependencies]] is zero-length HList.
    * */
  def baked(implicit ev: HNil =:= Dependencies): F[A] = bake(HNil)

  /**
    * Automatically pick-up dependencies from the call-site scope
    * and bake this cake!
    *
    * @see [[AutoBake]] for type class
    * @see [[cakeless.internal.AutoBaking]] for implementation
    * */
  def auto(implicit autoBake: AutoBake[F, A, Dependencies]): F[A] = autoBake(self)

}
