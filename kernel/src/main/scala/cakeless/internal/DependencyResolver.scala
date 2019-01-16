package cakeless.internal

import scala.language.higherKinds
import scala.language.experimental.macros
import shapeless._
import scala.reflect.macros.whitebox
import japgolly.microlibs.macro_utils.MacroUtils

/**
  * Whitebox-macro magic
  * picking up cake dependencies on the type level
  * and generating wiring code (like macwire does).
  * */
class DependencyResolver(val c: whitebox.Context) extends MacroUtils {

  import c.universe._

  protected val hnil = typeOf[HNil].dealias

  protected val debug = sys.env.getOrElse("cakeless_macro_debug", "false").toBoolean

  protected def assertMacro(cond: Boolean, msg: String): Unit =
    if (!cond) c.abort(c.enclosingPosition, msg)

  protected def ifDebug[U](thunk: => U): Unit =
    if (debug) thunk

  protected def buildHListType(returnTypes: List[Type]): Tree = {
    def buildHList(head: Type, remaining: List[Type]): Tree = remaining match {
      case Nil                  => tq"shapeless.::[$head, $hnil]"
      case scala.::(dep, rest0) => tq"shapeless.::[$head, ${buildHList(dep, rest0)}]"
    }

    val scala.::(first, rest) = returnTypes.map(_.resultType.dealias)
    buildHList(first, rest)
  }

  protected def extractClassesChain(tpe: Type): List[Type] = tpe match {
    case RefinedType(types, _) => types.flatMap(extractClassesChain)
    case _                     => tpe :: Nil
  }

  protected def instantiate[A: WeakTypeTag](
      mainType: Type,
      constructorParams: List[List[Type]],
      abstractValues: List[MethodSymbol],
      depsValueName: TermName,
      depsType: Tree
  ): Tree = {
    val A = weakTypeOf[A].dealias

    val typeRefinements = extractClassesChain(A).filterNot(_ =:= mainType)

    ifDebug {
      println("Self-types:\n\t" + typeRefinements.mkString("\n\t"))
      println(sep)
    }

    val (passConstructorParams, hlistOffset) = constructorParams
      .foldLeft(List.empty[List[Tree]] -> 0) {
        case ((paramsAcc, offset), carry) =>
          val carryAssignments: List[Tree] = carry.indices.map { idx =>
            c.untypecheck(q"""$depsValueName(${offset + idx})""")
          }.toList
          (paramsAcc :+ carryAssignments, offset + carry.size)
      }

    ifDebug {
      println(
        "Constructor params:\n\t" + passConstructorParams.mkString("\n\t")
      )
      println(sep)
    }

    val assignments = {
      abstractValues.zipWithIndex.map {
        case (method, idx) =>
          c.untypecheck(q"override lazy val ${method.name} = $depsValueName(${hlistOffset + idx})")
      }
    }

    ifDebug {
      println("Assignments:\n\t" + assignments.mkString("\n\t"))
      println(sep)
    }

    q"""
       new cakeless.Cake[$A] {
         type Dependencies = $depsType
         def bake($depsValueName: $depsType): $A = new $mainType(...$passConstructorParams) with ..$typeRefinements { ..$assignments }
       }"""
  }

  def makeCake0[A: WeakTypeTag]: Tree = makeCake[A](reify(0))

  def makeCake[A: WeakTypeTag](constructor: Expr[Int]): Tree = {
    val A = weakTypeOf[A].dealias

    val constNum: Int = constructor match {
      case Expr(Literal(Constant(n: Int))) =>
        assertMacro(n >= 0, s"`constructor` should not be less than 0")
        n
      case _ =>
        fail(s"`constructor` must be constant Int! Given: $constructor")
    }

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

    val abstractMembersReturnTypes = abstractValues.map(_.returnType)

    val mainType = A match {
      case RefinedType(types, _) => types.head
      case _                     => A
    }

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

          assertMacro(constNum < constructorsList.size,
                      s"Chosen $constNum constructor but found only ${constructorsList.size} constructors")

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
    val deps = buildHListType(constructorParams.flatMap(_.map(_.typeSignature.dealias)) ++ abstractMembersReturnTypes)

    val expr = instantiate[A](
      mainType,
      constructorParams.map(_.map(_.typeSignature.dealias)),
      abstractValues,
      TermName("deps"),
      deps
    )

    ifDebug {
      println(sep)
      println("Generated code:\n" + expr)
      println(sep)
    }

    expr
  }

  def makeCakeT0[F[_], A: WeakTypeTag](implicit _F: WeakTypeTag[F[_]]): Tree = makeCakeT[F, A](reify(0))

  def makeCakeT[F[_], A: WeakTypeTag](constructor: Expr[Int])(implicit _F: WeakTypeTag[F[_]]): Tree = {
    val F = _F.tpe.dealias.typeConstructor.etaExpand

    val baseCake = makeCake[A](constructor)

    val expr =
      q"""
       import cats.{~>, Id, Applicative}

       $baseCake.mapK[$F](new ~>[Id, $F] {
         def apply[A](fa: A) = Applicative[$F].pure(fa)
       })
     """

    ifDebug {
      println(sep)
      println(expr)
      println(sep)
    }

    expr
  }
}
