package cakeless.compiletime

import japgolly.microlibs.macro_utils.MacroUtils
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/**
  * Whitebox-macro magic
  * picking up cake dependencies on the type level
  * and generating wiring code (like macwire does).
 **/
abstract class DependencyResolver(val c: blackbox.Context) extends MacroUtils {

  import c.universe._

  case class ConstructorParamInfo(constructorNumber: Int)
  case class Dependency(prefixType: Type, name: Name, tpe: Type, constructorInfo: Option[ConstructorParamInfo])

  case class CakeInfo(
      A: Type,
      depsTypesList: List[Dependency],
      depsValueName: TermName,
      mainType: Type,
      passConstructorParamsIndexes: List[List[Int]],
      typeRefinements: List[Type],
      assignmentWithIndexes: List[(TermName, Int)],
      excludedTypes: List[Type],
      excludedDeps: List[MethodSymbol]
  )

  protected val debug: Boolean = sys.env.getOrElse("cakeless_macro_debug", "false").toBoolean

  protected def assertMacro(cond: Boolean, msg: String): Unit =
    if (!cond) c.abort(c.enclosingPosition, msg)

  protected def debugged[U](force: Boolean = debug, extraInfo: String = "")(thunk: => U): U = {
    val res = thunk
    if (force) println(s"[Cakeless] [Macro] $extraInfo $res")
    res
  }

  protected def extractClassesChain(tpe: Type): List[Type] = tpe match {
    case RefinedType(types, _) => types.flatMap(extractClassesChain)
    case _                     => tpe :: Nil
  }

  protected def getCakeInfo[A: WeakTypeTag](
      constructor: Int,
      refinementExclusions: Set[Type],
      depsExclusions: Set[Type]
  ): CakeInfo = {
    val A = weakTypeOf[A].dealias

    val (mainType, refinements, excludedTypes) = unrefine(A) match {
      case A =>
        val withSelfTypes = unrefine(A.typeSymbol.asClass.selfType).distinct.flatMap(unrefine).filterNot(isAny)
        val excl          = withSelfTypes.filter(tpe => refinementExclusions.exists(tpe.<:<)).distinct.flatMap(unrefine)
        (A, withSelfTypes, excl)

      case types =>
        val (excl, withExcludedTypes) =
          types.distinct.filterNot(isAny).partition(tpe => refinementExclusions.exists(tpe.<:<))
        val withSelfTypes = withExcludedTypes.map(_.typeSymbol.asClass.selfType).flatMap(unrefine).distinct
        val mt = withExcludedTypes
          .find(isClass)
          .orElse(withExcludedTypes.headOption)
          .getOrElse {
            fail(s"Type $A is a refined type containing only one of $refinementExclusions")
          }
        (mt, withSelfTypes, excl.flatMap(unrefine))
    }

    val abstractValues = refinements
      .map(tpe => tpe -> getNullaryAbstractMethods(tpe).filterNot(v => depsExclusions.exists(v.returnType.<:<)))

    val excludedMembers = excludedTypes.flatMap(getNullaryAbstractMethods)

    val abstractMembers = {
      for {
        (prefixType, avs) <- abstractValues
        av                <- avs
      } yield Dependency(prefixType = prefixType, name = av.name, tpe = av.returnType, constructorInfo = None)
    }

    debugged() {
      s"Main type: $mainType\n$sep"
    }

    val constructorParams = {
      if (mainType.decls.exists(s => s.isMethod && s.asMethod.isConstructor)) {
        if (constructor == 0)
          mainType.decls
            .collectFirst { case m: MethodSymbol if m.isPrimaryConstructor => m }
            .getOrElse(fail("Unable to discern primary constructor."))
            .paramLists
        else {
          val constructorsList = mainType.decls.collect { case m: MethodSymbol if m.isConstructor => m }.toList

          assertMacro(
            constructor < constructorsList.size,
            s"Chosen $constructor constructor but found only ${constructorsList.size} constructors"
          )

          constructorsList(constructor).paramLists
        }
      } else {
        if (constructor > 0)
          c.warning(c.enclosingPosition, s"$mainType doesn't have constructors at all but requested #$constructor constructor")
        Nil
      }
    }

    debugged() {
      s"#$constructor constructor params: $constructorParams\n$sep"
    }

    val allParamsList = constructorParams.flatMap(
      _.map(s =>
        Dependency(
          prefixType = mainType,
          name = s.name,
          tpe = s.typeSignature.dealias,
          constructorInfo = Some(ConstructorParamInfo(constructorNumber = constructor))
        )
      )
    ) ++ abstractMembers

    val typeRefinements = refinements.flatMap(extractClassesChain).filterNot(_ <:< mainType).distinct

    debugged() {
      "Self-types:\n\t" + typeRefinements.mkString("\n\t") + '\n' + sep
    }

    val depsValueName = TermName("deps")

    val (passConstructorParams, abstractValuesOffset) = constructorParams
      .foldLeft(List.empty[List[Int]] -> 0) {
        case ((paramsAcc, offset), carry) =>
          val carryIndexes: List[Int] = carry.indices.map(_ + offset).toList
          (paramsAcc :+ carryIndexes, offset + carry.size)
      }

    debugged() {
      "Constructor params:\n\t" + passConstructorParams.mkString("\n\t") + '\n' + sep
    }

    val assignmentIndexes = {
      abstractValues.flatMap(_._2).zipWithIndex.map {
        case (method, idx) =>
          (method.name, abstractValuesOffset + idx)
      }
    }

    debugged() {
      "Assignments:\n\t" + assignmentIndexes.mkString("\n\t") + '\n' + sep
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

  protected def unrefine(tpe: Type): List[Type] = tpe match {
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
