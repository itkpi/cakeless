package com.github.cakeless.internal

import scala.language.higherKinds
import scala.language.experimental.macros
import shapeless._
import scala.reflect.macros.whitebox

class DependencyResolver(val c: whitebox.Context) {
  import c.universe._

  private val hnil = typeOf[HNil].dealias

  def assertMacro(cond: Boolean, msg: String): Unit =
    if (!cond) c.abort(c.enclosingPosition, msg)

  def buildHListType[A: WeakTypeTag](returnTypes: List[Type]): Tree = {
    def buildHList(head: Type, remaining: List[Type]): Tree = remaining match {
      case Nil                  => tq"shapeless.::[$head, $hnil]"
      case scala.::(dep, rest0) => tq"shapeless.::[$head, ${buildHList(dep, rest0)}]"
    }

    val scala.::(first, rest) = returnTypes.map(_.resultType.dealias)
    buildHList(first, rest)
  }

  def instantiate[A: WeakTypeTag](
      abstractValues: List[MethodSymbol],
      depsValueName: TermName,
      depsType: Tree
  ): Tree = {
    val A = weakTypeOf[A].dealias

    val assignments = abstractValues.zipWithIndex.map {
      case (method, idx) =>
        c.untypecheck(q"override lazy val ${method.name} = $depsValueName($idx)")
    }

//    println(assignments.mkString("\n"))
    val clsName = TypeName(c.freshName(s"cakeless_${A.typeSymbol.name.toString}"))

    q"""
       new com.github.cakeless.Cake[$A] {
         type Dependencies = $depsType
         class $clsName() extends ${A.typeSymbol.asClass.selfType} { ..$assignments }
         def bake($depsValueName: $depsType): $A = new $clsName() {}
       }
     """
  }

  def makeCake[A: WeakTypeTag]: Tree = {
    val A = weakTypeOf[A].dealias

    val abstractValues = A.typeSymbol.asClass.selfType.members
      .filter(s => s.isMethod && s.isAbstract)
      .map(_.asMethod)
      .filter(_.paramLists.isEmpty)
      .toList
      .reverse

    assertMacro(
      abstractValues.nonEmpty,
      s"There is no sense in cakeless for $A because it doesn't have parameter-less abstract val's or def's"
    )

    val returnTypes = abstractValues.map(_.returnType)

    val deps = buildHListType[A](returnTypes)

    val expr = instantiate[A](abstractValues, TermName("deps"), deps)
    println(expr)
    expr
  }
}
