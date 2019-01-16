package cakeless.cats_effect

import cakeless.internal.DependencyResolver
import cats.effect.concurrent.Ref

import scala.reflect.macros.whitebox
import scala.language.experimental.macros
import scala.language.higherKinds

class SyncResolver(override val c: whitebox.Context) extends DependencyResolver(c) {
  import c.universe._

  def makeCakeSync0[F[_], A: WeakTypeTag](implicit _F: WeakTypeTag[F[_]]): Tree =
    makeCakeSyncImpl[F, A](reify { 0 }, isSingleton = false)

  def makeCakeSync[F[_], A: WeakTypeTag](constructor: Expr[Int])(implicit _F: WeakTypeTag[F[_]]): Tree =
    makeCakeSyncImpl[F, A](constructor, isSingleton = false)

  def makeCakeSyncSingleton0[F[_], A: WeakTypeTag](implicit _F: WeakTypeTag[F[_]]): Tree =
    makeCakeSyncImpl[F, A](reify { 0 }, isSingleton = true)

  def makeCakeSyncSingleton[F[_], A: WeakTypeTag](constructor: Expr[Int])(implicit _F: WeakTypeTag[F[_]]): Tree =
    makeCakeSyncImpl[F, A](constructor, isSingleton = true)

  def makeCakeSyncImpl[F[_], A: WeakTypeTag](constructor: Expr[Int], isSingleton: Boolean)(implicit _F: WeakTypeTag[F[_]]): Tree = {
    val F = _F.tpe.dealias.typeConstructor.etaExpand
    val A = weakTypeOf[A].dealias

    val baseCake = makeCake[A](constructor)

    val expr = if (!isSingleton) {
      q"""
       import cats.{~>, Id}
       import cats.effect.Sync

       new cakeless.CakeT[$F, $A] {
          val baseCake = $baseCake
          type Dependencies = baseCake.Dependencies
          def bake(deps: Dependencies) = Sync[$F].delay(baseCake bake deps)
       }
     """
    } else {
      q"""
       import cats.{~>, Id}
       import cats.effect.Sync
       import cats.effect.concurrent.Ref

       new cakeless.CakeT[$F, $A] {
          val baseCake = $baseCake
          type Dependencies = baseCake.Dependencies
          
          private val ref = Ref.unsafe[$F, Option[$A]](None)
          
          private val F = Sync[$F]
          
          def bake(deps: Dependencies) = F.flatMap(ref.get) {
            case None => 
              val fa = F.delay(baseCake bake deps)
              F.flatMap(fa) { a =>
                F.as(ref.set(Some(a)), a)
              }
            case Some(a) => F.pure(a)
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
}
