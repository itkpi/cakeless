package cakeless.internal

import cakeless.CakeZ
import scala.reflect.macros.whitebox

class ZioResolver(override val c: whitebox.Context) extends DependencyResolver(c) {
  import c.universe._

  def makeZSync0[A: WeakTypeTag]: Tree =
    makeCakeZImpl[A](reify { 0 }, isSingleton = false)

  def makeZSync[A: WeakTypeTag](constructor: Expr[Int]): Tree =
    makeCakeZImpl[A](constructor, isSingleton = false)

  def makeCakeZSingleton0[A: WeakTypeTag]: Tree =
    makeCakeZImpl[A](reify { 0 }, isSingleton = true)

  def makeCakeZSingleton[A: WeakTypeTag](constructor: Expr[Int]): Tree =
    makeCakeZImpl[A](constructor, isSingleton = true)

  def makeCakeZImpl[A: WeakTypeTag](constructor: Expr[Int], isSingleton: Boolean): Tree = {
    val A = weakTypeOf[A].dealias

    val baseCake = makeCake[A](constructor)

    val expr = if (!isSingleton) {
      baseCake
    } else {
      q"""
       import scalaz.zio._

       new _root_.cakeless.CakeZ[Any, Throwable, $A] {
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

  def makeCake0[A: WeakTypeTag]: Tree = makeCake[A](reify { 0 })

  def makeCake[A: WeakTypeTag](constructor: Expr[Int]): Tree = {
    val info = getCakeInfo[A](constructor)
    import info._

    q"""
       new _root_.cakeless.CakeZ[Any, Throwable, $A] {
         import scalaz.zio._

         final type Dependencies = $depsType

         def bake($depsValueName: $depsType): ZIO[Any, Throwable, $A] = ZIO { new $mainType(...$passConstructorParams) with ..$typeRefinements { ..$assignments } }
       }"""
  }
}
