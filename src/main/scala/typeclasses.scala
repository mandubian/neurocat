package neurocat
package typeclasses

import shapeless.{HList, HNil, ::, DepFn2}
import cats.Monoid

import algebra.ring.{MultiplicativeMonoid, AdditiveMonoid}

// import shapeless.ops.record._

// import shapeless.record._
import shapeless.labelled._
import shapeless._ ; import syntax.singleton._ ; import record._

import cats.evidence._

import singleton.ops._
import org.nd4j.linalg.factory.Nd4j
import nd4j._


// sealed trait Mat[S, D <: Dim2[_, _]]

// case class Matrix[S, D <: Dim2[_, _]]() extends Mat[S, D]
// case class Identity[S, R <: XInt]() extends Mat[S, R x R]
// case class One[S, D <: Dim2[_, _]]() extends Mat[S, D]
// case class Zero[S, D <: Dim2[_, _]]() extends Mat[S, D]
// case class Diag[S, R <: XInt]() extends Mat[S, R x R]


trait ParametrisedFunction[P, A, B] extends Function2[P, A, B]

trait ParametrisedDiff[P, A, B, GradP, GradA] extends ParametrisedFunction[P, A, B] {
  def gradP(p: P, a: A): GradP
  def gradA(p: P, a: A): GradA
}

trait Merger[P, Q]{
  type Out
  def left(p: Out): P
  def right(p: Out): Q
  def apply(p: P, q: Q): Out
} 

object Merger extends Merger1 {
  implicit def pMerger[P]: Merger.Aux[P, P, P] = new Merger[P, P] {
    type Out = P

    def left(p: P): P = p
    def right(p: P): P = p
    def apply(hnil: P, p: P): P = p
  }


}

trait Merger1 extends Merger2 {

  implicit def hnilMerger[P]: Merger.Aux[HNil, P, P] = new Merger[HNil, P] {
    type Out = P

    def left(pq: P): HNil = HNil
    def right(p: P): P = p
    def apply(hnil: HNil, p: P): P = p
  }
}

trait Merger2 extends Merger3 {

  implicit def hnil2Merger[P]: Merger.Aux[P, HNil, P] = new Merger[P, HNil] {
    type Out = P

    def left(p: P): P = p
    def right(p: P): HNil = HNil
    def apply(p: P, hnil: HNil): P = p
  }

  implicit def mergerSwitch[P, Q, PQ](implicit m: Merger.Aux[P, Q, PQ]): Merger.Aux[Q, P, PQ] = new Merger[Q, P] {
    type Out = PQ

    def left(pq: PQ): Q = m.right(pq)
    def right(pq: PQ): P = m.left(pq)
    def apply(q: Q, p: P): PQ = m(p, q)
  }

  implicit def remergerLeft[P, Q, PQ](implicit m: Merger.Aux[P, Q, PQ]): Merger.Aux[PQ, P, PQ] = new Merger[PQ, P] {
    type Out = PQ

    def left(pq: PQ): PQ = pq
    def right(pq: PQ): P = m.left(pq)
    // overwrite P?
    def apply(pq: PQ, p: P): PQ = m.apply(p, m.right(pq))
  }
}

trait Merger3 {
  type Aux[P, Q, PQ0] = Merger[P, Q] { type Out = PQ0 }

  implicit def hlistMerger[P <: HList, Q <: HList](
    implicit m: MergerE[P, Q]
  ): Merger.Aux[P, Q, m.Out] = new Merger[P, Q] {
    type Out = m.Out

    def left(pq: Out): P = m.left(pq)
    def right(pq: Out): Q = m.right(pq)
    def apply(p: P, q: Q): Out = m(p, q)
  }


  implicit def remergerRight[P, Q, PQ](implicit m: Merger.Aux[P, Q, PQ]): Merger.Aux[PQ, Q, PQ] = new Merger[PQ, Q] {
    type Out = PQ

    def left(pq: PQ): PQ = pq
    def right(pq: PQ): Q = m.right(pq)
    // overwrite P?
    def apply(pq: PQ, q: Q): PQ = m.apply(m.left(pq), q)
  }

}

trait Norm[A, S] {
  def norm: S
}


trait Zeroed[A] {
  def zero: A
}

object Zeroed {
  def apply[A](implicit zeroed: Zeroed[A]) = zeroed
}


trait Oned[A] {
  def one: A
}

object Oned {
  def apply[A](implicit oned: Oned[A]) = oned
}


trait DiagOned[A] {
  def diagOne: A
}

object DiagOned {
  def apply[A](implicit diagOned: DiagOned[A]) = diagOned
}


trait TypeCompiler[P] {
  type Out
  def compile(p: P): Out
}
object TypeCompiler {
  type Aux[P, O] = TypeCompiler[P] { type Out = O }
}


trait Tuplizer[P, A] {
  type Out

  def tuplize(p: P, a: A): Out
  def untuplize(pa: Out): (P, A)
}

object Tuplizer extends TuplizerImplicits{

  implicit def tupl0[A] = new Tuplizer[HNil, A] {
    type Out = A
    def tuplize(p: HNil, a: A): A = a
    def untuplize(pa: A): (HNil, A) = (HNil, pa)
  }
}

trait TuplizerImplicits {
  type Aux[P, A, O] = Tuplizer[P, A] { type Out = O }

  implicit def tupl1[P, A] = new Tuplizer[P, A] {
    type Out = (P, A)
    def tuplize(p: P, a: A): (P, A) = (p, a)
    def untuplize(pa: (P, A)): (P, A) = pa
  }
}

