package com.github.cakeless.internal

import com.github.cakeless.Cake

import scala.language.higherKinds
import scala.language.experimental.macros
import shapeless._
import scala.reflect.macros.whitebox

class DependencyResolver(val c: whitebox.Context) {
  import c.universe._

  private val hnil = typeOf[HNil].dealias

  def assertMacro(cond: Boolean, msg: String): Unit =
    if (!cond) c.abort(c.enclosingPosition, msg)

  def resolveDependencies[A: WeakTypeTag]: Tree = {
    def buildHList(head: Type, remaining: List[Type]): Tree = remaining match {
      case Nil                  => tq"shapeless.::[$head, $hnil]"
      case scala.::(dep, rest0) => tq"shapeless.::[$head, ${buildHList(dep, rest0)}]"
    }

    val A = weakTypeOf[A]

    val abstractValues = A.members
      .filter(s => s.isMethod && s.isAbstract)
      .map(_.asMethod)
      .filter(_.paramLists.isEmpty)

    assertMacro(
      abstractValues.nonEmpty,
      s"There is no sense in cakeless for $A because it doesn't have parameter-less abstract val's or def's"
    )

    val returnTypes = abstractValues.map(_.returnType)

    val scala.::(first, rest) = returnTypes.map(_.resultType.dealias).toList.reverse
    buildHList(first, rest)
  }

  def makeCake[A: WeakTypeTag]: Tree = {
    val A    = weakTypeOf[A].dealias
    val deps = resolveDependencies[A]

//    println(s"Deps for $A: $deps")
    val expr = q"""
       new com.github.cakeless.Cake[$A] {
         type Dependencies = $deps
         def bake(deps: Dependencies): $A = null
       }
     """
//    println(expr)
    expr
  }
}
