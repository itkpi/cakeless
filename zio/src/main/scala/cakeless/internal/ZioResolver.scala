package cakeless.internal

import cakeless.CakeZ
import scala.reflect.macros.whitebox

class ZioResolver(override val c: whitebox.Context) extends DependencyResolver(c) {
  import c.universe._

  def makeZSync0[Effect: WeakTypeTag, Error: WeakTypeTag, A: WeakTypeTag]: Tree =
    makeCakeZImpl[Effect, Error, A](reify { 0 }, isSingleton = false)

  def makeZSync[Effect: WeakTypeTag, Error: WeakTypeTag, A: WeakTypeTag](constructor: Expr[Int]): Tree =
    makeCakeZImpl[Effect, Error, A](constructor, isSingleton = false)

  def makeCakeZSingleton0[Effect: WeakTypeTag, Error: WeakTypeTag, A: WeakTypeTag]: Tree =
    makeCakeZImpl[Effect, Error, A](reify { 0 }, isSingleton = true)

  def makeCakeZSingleton[Effect: WeakTypeTag, Error: WeakTypeTag, A: WeakTypeTag](constructor: Expr[Int]): Tree =
    makeCakeZImpl[Effect, Error, A](constructor, isSingleton = true)

  def makeCakeZImpl[Effect: WeakTypeTag, Error: WeakTypeTag, A: WeakTypeTag](constructor: Expr[Int], isSingleton: Boolean): Tree = {
    val Effect = weakTypeOf[Effect].dealias
    val Error  = weakTypeOf[Error].dealias
    val A      = weakTypeOf[A].dealias

    val baseCake = makeCake[Effect, Error, A](constructor)

    val expr = if (!isSingleton) {
      baseCake
    } else {
      q"""
       import scalaz.zio._

       new _root_.cakeless.CakeZ[$Effect, $Error, $A] {
          val baseCake = $baseCake
          type Dependencies = baseCake.Dependencies

          private val runtime = new DefaultRuntime {}
          private val ref = runtime unsafeRun Ref.make[Option[$A]](None)

          def bake(deps: Dependencies) = 
            ref.get.flatMap {
              case None =>
                val fa = baseCake bake deps
                fa flatMap { a =>
                  ref.set(Some(a)) *> ZIO.succeed(a)
                }
              case Some(a) => ZIO.succeed(a)
            }
       }
     """

    }

    ifDebug {
      println(sep)
      println(expr)
      println(sep)
    }

    expr
  }

  def makeCake0[Effect: WeakTypeTag, Error: WeakTypeTag, A: WeakTypeTag]: Tree = makeCake[Effect, Error, A](reify { 0 })

  def makeCake[Effect: WeakTypeTag, Error: WeakTypeTag, A: WeakTypeTag](constructor: Expr[Int]): Tree = {
    val Effect = weakTypeOf[Effect].dealias
    val Error  = weakTypeOf[Error].dealias
    val info   = getCakeInfo[A](constructor)
    import info._

    q"""
       new _root_.cakeless.CakeZ[$Effect, $Error, $A] {
         import scalaz.zio._

         final type Dependencies = $depsType

         def bake($depsValueName: $depsType): ZIO[$Effect, $Error, $A] = ZIO.succeedLazy(new $mainType(...$passConstructorParams) with ..$typeRefinements { ..$assignments })
       }"""
  }
}
