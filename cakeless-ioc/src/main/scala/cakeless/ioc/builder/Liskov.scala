package cakeless.ioc.builder

import scala.annotation.implicitNotFound

/**
  * From scalaz
  *  @link - https://scalaz.github.io/scalaz/scalaz-2.9.0-1-6.0/doc.sxr/scalaz/Liskov.scala.html
  * */
@implicitNotFound("""Cannot prove that ${A} is a subtype of ${B}.""")
sealed trait Liskov[-A, +B] {
  def subst[F[-_]](p: F[B]): F[A]

  final def *[+[+_, +_], C, D](that: Liskov[C, D]): Liskov[A + C, B + D] = Liskov.lift2(this, that)

  final def andThen[C](that: Liskov[B, C]): Liskov[A, C] = Liskov.trans(that, this)

  final def compose[C](that: Liskov[C, A]): Liskov[C, B] = Liskov.trans(this, that)
}

object Liskov {
  implicit def isa[A, B >: A]: A <| B = new (A <| B) {
    def subst[F[-_]](p: F[B]): F[A] = p
  }

  /**Subtyping is reflexive */
  implicit def refl[A]: (A <| A) = new (A <| A) {
    def subst[F[_]](p: F[A]): F[A] = p
  }

  /**Subtyping is transitive */
  def trans[A, B, C](f: B <| C, g: A <| B): A <| C =
    g.subst[λ[`-α` => α <| C]](f)

  def antisim[A, B](ev: A <| B): A |> B = ev.asInstanceOf[A |> B]

  /**lift2(a,b) = co1_2(a) compose co2_2(b) */
  def lift2[T[+_, +_], A, A2, B, B2](
      a: A <| A2,
      b: B <| B2
  ): (T[A, B] <| T[A2, B2]) = {
    type a[-X] = T[X, B2] <| T[A2, B2]
    type b[-X] = T[A, X] <| T[A2, B2]
    b.subst[b](a.subst[a](refl))
  }
}
