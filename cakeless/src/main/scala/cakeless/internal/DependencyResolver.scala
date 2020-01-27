package cakeless.internal

import japgolly.microlibs.macro_utils.MacroUtils
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
  * Whitebox-macro magic
  * picking up cake dependencies on the type level
  * and generating wiring code (like macwire does).
 **/
abstract class DependencyResolver(val c: whitebox.Context) extends MacroUtils {

  import c.universe._

  case class CakeInfo(
      A: Type,
      depsTypesList: List[Type],
      depsValueName: TermName,
      mainType: Type,
      passConstructorParamsIndexes: List[List[Int]],
      typeRefinements: List[Type],
      assignmentWithIndexes: List[(TermName, Int)],
      excludedTypes: List[Type],
      excludedDeps: List[MethodSymbol]
  )

  protected val debug = sys.env.getOrElse("cakeless_macro_debug", "false").toBoolean

  protected def assertMacro(cond: Boolean, msg: String): Unit =
    if (!cond) c.abort(c.enclosingPosition, msg)

  protected def ifDebug[U](thunk: => U): Unit =
    if (debug) thunk

  protected def extractClassesChain(tpe: Type): List[Type] = tpe match {
    case RefinedType(types, _) => types.flatMap(extractClassesChain)
    case _                     => tpe :: Nil
  }

  protected def getCakeInfo[A: WeakTypeTag](
      constructor: Expr[Int],
      refinementExclusions: Set[Type],
      depsExclusions: Set[Type]
  ): CakeInfo = {
    val A = weakTypeOf[A].dealias

    val constNum: Int = constructor match {
      case Expr(Literal(Constant(n: Int))) =>
        assertMacro(n >= 0, s"`constructor` should not be less than 0")
        n
      case _ =>
        fail(s"`constructor` must be constant Int! Given: $constructor")
    }

    val (mainType, refinements, excludedTypes) = A match {
      case RefinedType(types, x) =>
        val (excl, withExcludedTypes) =
          types.flatMap(unrefine).distinct.filterNot(isAny).partition(tpe => refinementExclusions.exists(tpe.<:<))
        val withSelfTypes = withExcludedTypes.map(_.typeSymbol.asClass.selfType).flatMap(unrefine).distinct
        val mt = withExcludedTypes
          .find(isClass)
          .orElse(withExcludedTypes.headOption)
          .getOrElse {
            fail(s"Type $A is a refined type containing only one of $refinementExclusions")
          }
        (mt, withSelfTypes, excl.flatMap(unrefine))
      case _ =>
        val withSelfTypes = unrefine(A.typeSymbol.asClass.selfType).distinct.flatMap(unrefine).filterNot(isAny)
        val excl          = withSelfTypes.filter(tpe => refinementExclusions.exists(tpe.<:<)).distinct.flatMap(unrefine)
        (A, withSelfTypes, excl)
    }

    val abstractValues = refinements
      .flatMap(getNullaryAbstractMethods)
      .filterNot(v => depsExclusions.exists(v.returnType.<:<))

    val excludedMembers = excludedTypes.flatMap(getNullaryAbstractMethods)

    assertMacro(
      abstractValues.nonEmpty,
      s"There is no sense in cakeless for $A because it doesn't have parameter-less abstract val's or def's"
    )

    val abstractMembersReturnTypes = abstractValues.map(_.returnType)

    ifDebug {
      println(s"Main type: $mainType")
      println(sep)
    }

    val constructorParams = {
      if (mainType.decls.exists(s => s.isMethod && s.asMethod.isConstructor)) {
        if (constNum == 0)
          mainType.decls
            .collectFirst { case m: MethodSymbol if m.isPrimaryConstructor => m }
            .getOrElse(fail("Unable to discern primary constructor."))
            .paramLists
        else {
          val constructorsList = mainType.decls.collect { case m: MethodSymbol if m.isConstructor => m }.toList

          assertMacro(
            constNum < constructorsList.size,
            s"Chosen $constNum constructor but found only ${constructorsList.size} constructors"
          )

          constructorsList(constNum).paramLists
        }
      } else {
        if (constNum > 0) c.warning(c.enclosingPosition, s"$mainType doesn't have constructors at all but requested #$constNum constructor")
        Nil
      }
    }

    ifDebug {
      println(s"#$constNum constructor params: $constructorParams")
      println(sep)
    }

    val allParamsList = constructorParams.flatMap(_.map(_.typeSignature.dealias)) ++ abstractMembersReturnTypes

    val typeRefinements = refinements.flatMap(extractClassesChain).filterNot(_ <:< mainType).distinct

    ifDebug {
      println("Self-types:\n\t" + typeRefinements.mkString("\n\t"))
      println(sep)
    }

    val depsValueName = TermName("deps")

    val (passConstructorParams, abstractValuesOffset) = constructorParams
      .foldLeft(List.empty[List[Int]] -> 0) {
        case ((paramsAcc, offset), carry) =>
          val carryIndexes: List[Int] = carry.indices.map(_ + offset).toList
          (paramsAcc :+ carryIndexes, offset + carry.size)
      }

    ifDebug {
      println(
        "Constructor params:\n\t" + passConstructorParams.mkString("\n\t")
      )
      println(sep)
    }

    val assignmentIndexes = {
      abstractValues.zipWithIndex.map {
        case (method, idx) =>
          (method.name, abstractValuesOffset + idx)
      }
    }

    ifDebug {
      println("Assignments:\n\t" + assignmentIndexes.mkString("\n\t"))
      println(sep)
    }

    CakeInfo(
      A,
      allParamsList,
      depsValueName,
      mainType,
      passConstructorParams,
      typeRefinements,
      assignmentIndexes,
      excludedTypes,
      excludedMembers
    )
  }

  private def unrefine(tpe: Type): List[Type] = tpe match {
    case RefinedType(types, _) => types.flatMap(unrefine)
    case _                     => List(tpe)
  }

  private def isAny(t: Type): Boolean = t.dealias =:= typeOf[Any].dealias

  private def isClass(t: Type): Boolean = {
    val tsym = t.typeSymbol
    tsym.isClass && !tsym.asClass.isTrait
  }

  private def getNullaryAbstractMethods(tpe: Type): List[MethodSymbol] =
    tpe.members
      .filter(s => s.isMethod && s.isAbstract)
      .map(_.asMethod)
      .filter(_.paramLists.isEmpty)
      .toList
}
