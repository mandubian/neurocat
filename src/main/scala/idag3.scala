package neurocat
package idag3

import shapeless.{HList, HNil, ::, DepFn2}
import cats.Monoid

import algebra.ring.{MultiplicativeMonoid, AdditiveMonoid}

import shapeless.ops.record._

// import shapeless.record._
import shapeless.labelled._
import shapeless._ ; import syntax.singleton._ ; import record._

import singleton.ops._
import org.nd4j.linalg.factory.Nd4j
import nd4j._

trait ParametrisedFunction[P, A, B] extends Function2[P, A, B]

trait ParametrisedDiff[P, A, B, GradP, GradA] extends ParametrisedFunction[P, A, B] {

  def gradP(p: P, a: A): GradP

  def gradA(p: P, a: A): GradA
}


sealed trait PDag[P <: HList, A, B] extends ParametrisedFunction[P, A, B]


case class ParametrisedDag[P <: HList, A, B](f: (P, A) => B) extends PDag[P, A, B] {
  def apply(p: P, a: A): B = f(p, a)
}

sealed trait Dag[A, B] extends PDag[HNil, A, B] with (A => B)

case class FunctionDag[A, B](f: A => B) extends Dag[A, B] {
  def apply(p: HNil, a: A): B = f(a)
  def apply(a: A): B = f(a)
}

case class DiffDag[A, B](f: Differentiable[A, B]) extends Dag[A, B] {
  def apply(p: HNil, a: A): B = f(a)
  def apply(a: A): B = f(a)

  def diff(a: A): B = f.diff(a)

  def diffDag: Dag[A, B] = FunctionDag(diff _)
}

sealed trait DiffPDag[P <: HList, A, B, GradP <: HList, GradA] extends PDag[P, A, B] with ParametrisedDiff[P, A, B, GradP, GradA] {
  self =>
  def gradPDag: ParametrisedDag[P, A, GradP] = ParametrisedDag((p:P, a:A) => self.gradP(p, a))

  def gradADag: ParametrisedDag[P, A, GradA] = ParametrisedDag((p:P, a:A) => self.gradA(p, a))
}

case class BasicDiffPDag[P <: HList, A, B, GradP <: HList, GradA](f: ParametrisedDiff[P, A, B, GradP, GradA])
extends DiffPDag[P, A, B, GradP, GradA] {
  def apply(p: P, a: A): B = f(p, a)

  def gradP(p: P, a: A): GradP = f.gradP(p, a)
  override def gradPDag: ParametrisedDag[P, A, GradP] = ParametrisedDag((p:P, a:A) => f.gradP(p, a))

  def gradA(p: P, a: A): GradA = f.gradA(p, a)
  override def gradADag: ParametrisedDag[P, A, GradA] = ParametrisedDag((p:P, a:A) => f.gradA(p, a))
}


final case class ParallelDiffPDag[
  P <: HList, A, B, GradP <: HList, GradA
, Q <: HList, C, D, GradQ <: HList, GradB
, PQ <: HList
, GradPQ <: HList
](
  f: DiffPDag[P, A, C, GradP, GradA], g: DiffPDag[Q, B, D, GradQ, GradB]
)(implicit 
    mergePQ: MergerE.Aux[P, Q, PQ]
  , mergeGradPQ: MergerE.Aux[GradP, GradQ, GradPQ]
) extends DiffPDag[PQ, (A, B), (C, D), GradPQ, (GradA, GradB)] {

  def apply(pq: PQ, ab: (A, B)): (C, D) = {
    (f(mergePQ.left(pq), ab._1), g(mergePQ.right(pq), ab._2))
  }

  def gradP(pq: PQ, ab: (A, B)): GradPQ =
    mergeGradPQ(f.gradP(mergePQ.left(pq), ab._1), g.gradP(mergePQ.right(pq), ab._2))

  def gradA(pq: PQ, ab: (A, B)): (GradA, GradB) =
    (f.gradA(mergePQ.left(pq), ab._1), g.gradA(mergePQ.right(pq), ab._2))
}

final case class ComposeDiffPDag[
  P <: HList, A, B, GradP <: HList, GradA
, Q <: HList, C, GradQ <: HList, GradB
, PQ <: HList
, GradPQ <: HList
](
  g: DiffPDag[Q, B, C, GradQ, GradB], f: DiffPDag[P, A, B, GradP, GradA]
)(implicit 
    mergerPQ: MergerE.Aux[P, Q, PQ]
  , mergerGradPQ: MergerE.Aux[GradP, GradQ, GradPQ]
) extends DiffPDag[PQ, A, C, GradPQ, (GradA, GradB)] {

  def apply(pq: PQ, a: A): C = g(mergerPQ.right(pq), f(mergerPQ.left(pq), a))

  def gradP(pq: PQ, a: A): GradPQ = {
    val d = f.gradP(mergerPQ.left(pq), a)
    mergerGradPQ(f.gradP(mergerPQ.left(pq), a), g.gradP(mergerPQ.right(pq), f(mergerPQ.left(pq), a)))
  }

  def gradA(pq: PQ, a: A): (GradA, GradB) = {
    val d = f.gradA(mergerPQ.left(pq), a)
    (f.gradA(mergerPQ.left(pq), a), g.gradA(mergerPQ.right(pq), f(mergerPQ.left(pq), a)))
  }
}


object DiffPDag {
  
  def par[
    P <: HList, A, C, GradP <: HList, GradA
  , Q <: HList, B, D, GradQ <: HList, GradB
  , PQ <: HList](
    f: DiffPDag[P, A, C, GradP, GradA]
  , g: DiffPDag[Q, B, D, GradQ, GradB]
  )(implicit
      mergerPQ: MergerE[P, Q]
    , mergerGradPQ: MergerE[GradP, GradQ]
  ): DiffPDag[mergerPQ.Out, (A, B), (C, D), mergerGradPQ.Out, (GradA, GradB)] =
    ParallelDiffPDag(f, g)(mergerPQ, mergerGradPQ)

  def compose[
    P <: HList, A, B, GradP <: HList, GradA
  , Q <: HList, C, GradQ <: HList, GradB
  , PQ <: HList](
    g: DiffPDag[Q, B, C, GradQ, GradB]
  , f: DiffPDag[P, A, B, GradP, GradA]
  )(implicit
      mergerPQ: MergerE[P, Q]
    , mergerGradPQ: MergerE[GradP, GradQ]
  ): DiffPDag[mergerPQ.Out, A, C, mergerGradPQ.Out, (GradA, GradB)] =
    ComposeDiffPDag(g, f)(mergerPQ, mergerGradPQ)

  def id[A : MultiplicativeMonoid]() = BasicDiffPDag[HNil, A, A, HNil, A](
    new ParametrisedDiff[HNil, A, A, HNil, A]{
      def apply(h:HNil, a: A): A = a

      def gradP(p: HNil, a: A): HNil = HNil

      def gradA(p: HNil, a: A): A = MultiplicativeMonoid[A].one
    }
  )

  def split[A : MultiplicativeMonoid : Comonoid] = BasicDiffPDag[HNil, A, (A, A), HNil, A](
    new ParametrisedDiff[HNil, A, (A, A), HNil, A]{
      def apply(h:HNil, a: A): (A, A) = Comonoid[A].split(a)

      def gradP(p: HNil, a: A): HNil = HNil

      def gradA(p: HNil, a: A): A = MultiplicativeMonoid[A].one
    }
  )

  def join[A : MultiplicativeMonoid : AdditiveMonoid] = BasicDiffPDag[HNil, (A, A), A, HNil, (A, A)](
    new ParametrisedDiff[HNil, (A, A), A, HNil, (A, A)]{
      def apply(h:HNil, a: (A, A)): A = AdditiveMonoid[A].plus(a._1, a._2)

      def gradP(p: HNil, a: (A, A)): HNil = HNil

      def gradA(p: HNil, a: (A, A)): (A, A) = (MultiplicativeMonoid[A].one, MultiplicativeMonoid[A].one)
    }
  )
  
  implicit class Ops[P <: HList, A, B, GradP <: HList, GradA](idag: DiffPDag[P, A, B, GradP, GradA]) {
    def ||[Q <: HList, C, D, GradQ <: HList, GradC](other: DiffPDag[Q, C, D, GradQ, GradC])(
      implicit mergerPQ: MergerE[P, Q], mergerGradPQ: MergerE[GradP, GradQ]
    ): DiffPDag[mergerPQ.Out, (A, C), (B, D), mergerGradPQ.Out, (GradA, GradC)] = par(idag, other)

    def >>>[Q <: HList, C, GradQ <: HList, GradB](other: DiffPDag[Q, B, C, GradQ, GradB])(
      implicit mergerPQ: MergerE[P, Q], mergerGradPQ: MergerE[GradP, GradQ]
    ): DiffPDag[mergerPQ.Out, A, C, mergerGradPQ.Out, (GradA, GradB)] = compose(other, idag)
  }

  def f[A:MultiplicativeMonoid] = (id[A] || id[A]) >>> (id[A] || id[A])

  // implicit val i = new MultiplicativeMonoid[Int] {
  //   def one = 1
  //   def times(a: Int, b: Int) = a * b
  // }
  // f[Int].apply(HNil, (5, 5))

  def Sigmoid[R, D <: Dim2[_, _]]: DiffPDag[HNil, Mat[R, D], Mat[R, D], HNil, Mat[R, D]] = BasicDiffPDag[HNil, Mat[R, D], Mat[R, D], HNil, Mat[R, D]](
    new ParametrisedDiff[
      HNil, Mat[R, D], Mat[R, D]
    , HNil, Mat[R, D]
    ] {
      def apply(p: HNil, a: Mat[R, D]) = new Mat[R, D](
        Nd4j.getExecutioner().execAndReturn(new org.nd4j.linalg.api.ops.impl.transforms.Sigmoid(a.value))
      )

      def gradP(p: HNil, a: Mat[R, D]): HNil = HNil

      def gradA(p: HNil, a: Mat[R, D]) = new Mat[R, D](
        Nd4j.getExecutioner().execAndReturn(new org.nd4j.linalg.api.ops.impl.transforms.SigmoidDerivative(a.value))
      )

    }
  )

  def Dense[R, InR <: XInt, OutR <: XInt, OutC <: XInt](s: String): DiffPDag[
      FieldType[s.type, Mat[R, OutR x InR]] :: HNil
    , Mat[R, InR x OutC]
    , Mat[R, OutR x OutC]
    , Mat[R, InR x OutC] :: HNil
    , Mat[R, OutR x InR]
  ] = BasicDiffPDag(
      new ParametrisedDiff[
        FieldType[s.type, Mat[R, OutR x InR]] :: HNil
      , Mat[R, InR x OutC]
      , Mat[R, OutR x OutC]
      , Mat[R, InR x OutC] :: HNil
      , Mat[R, OutR x InR]
      ] {
        def apply(weights: FieldType[s.type, Mat[R, OutR x InR]] :: HNil, in: Mat[R, InR x OutC]): Mat[R, OutR x OutC] = {
          new Mat[R, OutR x OutC](weights.head.value.mmul(in.value))
        }

        def gradP(weights: FieldType[s.type, Mat[R, OutR x InR]] :: HNil, in: Mat[R, InR x OutC]): Mat[R, InR x OutC] :: HNil = {
          in :: HNil
        }

        def gradA(weights: FieldType[s.type, Mat[R, OutR x InR]] :: HNil, in: Mat[R, InR x OutC]): Mat[R, OutR x InR] = {
          weights.head
        }
      }
    )

  def nnetlayer[R, InR <: XInt, OutR <: XInt, OutC <: XInt] = 
    Dense[R, InR, OutR, OutC]("s") || Dense[R, InR, OutR, OutC]("s") //) >>> Sigmoid
}
