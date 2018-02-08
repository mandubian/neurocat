package neurocat
package idag4

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

sealed trait PDag[P, A, B] extends ParametrisedFunction[P, A, B]

case class ParametrisedDag[P, A, B](f: (P, A) => B) extends PDag[P, A, B] {
  def apply(p: P, a: A): B = f(p, a)
}

sealed trait DiffPDag[P, A, B, GradP, GradA] extends ParametrisedDiff[P, A, B, GradP, GradA] {
  self =>
  def gradPDag: ParametrisedDag[P, A, GradP] = ParametrisedDag((p:P, a:A) => self.gradP(p, a))

  def gradADag: ParametrisedDag[P, A, GradA] = ParametrisedDag((p:P, a:A) => self.gradA(p, a))
}

case class BasicDiffPDag[P, A, B, GradP, GradA](f: ParametrisedDiff[P, A, B, GradP, GradA])
extends DiffPDag[P, A, B, GradP, GradA] {
  def apply(p: P, a: A): B = f(p, a)

  def gradP(p: P, a: A): GradP = f.gradP(p, a)
  override def gradPDag: ParametrisedDag[P, A, GradP] = ParametrisedDag((p:P, a:A) => f.gradP(p, a))

  def gradA(p: P, a: A): GradA = f.gradA(p, a)
  override def gradADag: ParametrisedDag[P, A, GradA] = ParametrisedDag((p:P, a:A) => f.gradA(p, a))
}


trait Merge[P, Q]{
  type PQ
  def left(p: PQ): P
  def right(p: PQ): Q
  def apply(p: P, q: Q): PQ
} 

object Merge {
  type Aux[P, Q, PQ0] = Merge[P, Q] { type PQ = PQ0 }

  implicit def hlistMerge[P <: HList, Q <: HList](
    implicit m: MergerE[P, Q]
  ): Merge.Aux[P, Q, m.Out] = new Merge[P, Q] {
    type PQ = m.Out

    def left(pq: PQ): P = m.left(pq)
    def right(pq: PQ): Q = m.right(pq)
    def apply(p: P, q: Q): PQ = m(p, q)
  }
}

case class ParallelDiffPDag[
  P, A, C, GP1, GA1
, Q, B, D, GP2, GA2
, PQ
, GP
](
  val f: DiffPDag[P, A, C, GP1, GA1]
, val g: DiffPDag[Q, B, D, GP2, GA2]
)(implicit
  mergePQ: Merge.Aux[P, Q, PQ]
, mergeGradPQ: Merge.Aux[GP1, GP2, GP]
) extends DiffPDag[PQ, (A, B), (C, D), GP, (GA1, GA2)] {

  def apply(pq: PQ, ab: (A, B)): (C, D) = {
    (f(mergePQ.left(pq), ab._1), g(mergePQ.right(pq), ab._2))
  }

  type GradP = GP
  def gradP(pq: PQ, ab: (A, B)): GP =
    mergeGradPQ(f.gradP(mergePQ.left(pq), ab._1), g.gradP(mergePQ.right(pq), ab._2))

  type GradA = (GA1, GA2)
  def gradA(pq: PQ, ab: (A, B)): (GA1, GA2) =
    (f.gradA(mergePQ.left(pq), ab._1), g.gradA(mergePQ.right(pq), ab._2))
}

final case class ComposeDiffPDag[
  P, A, B, GP1, GA1
, Q, C, GP2, GA2
, PQ
, GP
](
  g: DiffPDag[Q, B, C, GP2, GA2], f: DiffPDag[P, A, B, GP1, GA1]
)(implicit 
    mergerPQ: Merge.Aux[P, Q, PQ]
  , mergerGradPQ: Merge.Aux[GP1, GP2, GP]
) extends DiffPDag[PQ, A, C, GP, (GA1, GA2)] {

  def apply(pq: PQ, a: A): C = g(mergerPQ.right(pq), f(mergerPQ.left(pq), a))

  def gradP(pq: PQ, a: A): GP = {
    val d = f.gradP(mergerPQ.left(pq), a)
    mergerGradPQ(f.gradP(mergerPQ.left(pq), a), g.gradP(mergerPQ.right(pq), f(mergerPQ.left(pq), a)))
  }

  def gradA(pq: PQ, a: A): (GA1, GA2) = {
    val d = f.gradA(mergerPQ.left(pq), a)
    (f.gradA(mergerPQ.left(pq), a), g.gradA(mergerPQ.right(pq), f(mergerPQ.left(pq), a)))
  }
}


object DiffPDag {

  def par[
    P, A, C, GP1, GA1
  , Q, B, D, GP2, GA2](
    f: DiffPDag[P, A, C, GP1, GA1]
  , g: DiffPDag[Q, B, D, GP2, GA2]
  )(
    implicit mergePQ0: Merge[P, Q], mergeGradPQ0: Merge[GP1, GP2]
  ): ParallelDiffPDag[
      P, A, C, GP1, GA1
    , Q, B, D, GP2, GA2
    , mergePQ0.PQ, mergeGradPQ0.PQ
  ] =
    ParallelDiffPDag(f, g)(mergePQ0, mergeGradPQ0)

  def compose[
    P, A, B, GP1, GA1
  , Q, C, GP2, GA2
  , PQ](
    g: DiffPDag[Q, B, C, GP2, GA2]
  , f: DiffPDag[P, A, B, GP1, GA1]
  )(implicit
      mergerPQ: Merge[P, Q]
    , mergerGradPQ: Merge[GP1, GP2]
  ): ComposeDiffPDag[
      P, A, B, GP1, GA1
    , Q, C, GP2, GA2
    , mergerPQ.PQ
    , mergerGradPQ.PQ
  ] = ComposeDiffPDag(g, f)(mergerPQ, mergerGradPQ)

  // def id[A : MultiplicativeMonoid]() = BasicDiffPDag[HNil, A, A, HNil, A](
  //   new ParametrisedDiff[HNil, A, A, HNil, A]{
  //     def apply(h:HNil, a: A): A = a

  //     def gradP(p: HNil, a: A): HNil = HNil

  //     def gradA(p: HNil, a: A): A = MultiplicativeMonoid[A].one
  //   }
  // )

//   def split[A : MultiplicativeMonoid : Comonoid] = BasicDiffPDag[HNil, A, (A, A), HNil, A](
//     new ParametrisedDiff[HNil, A, (A, A), HNil, A]{
//       def apply(h:HNil, a: A): (A, A) = Comonoid[A].split(a)

//       def gradP(p: HNil, a: A): HNil = HNil

//       def gradA(p: HNil, a: A): A = MultiplicativeMonoid[A].one
//     }
//   )

//   def join[A : MultiplicativeMonoid : AdditiveMonoid] = BasicDiffPDag[HNil, (A, A), A, HNil, (A, A)](
//     new ParametrisedDiff[HNil, (A, A), A, HNil, (A, A)]{
//       def apply(h:HNil, a: (A, A)): A = AdditiveMonoid[A].plus(a._1, a._2)

//       def gradP(p: HNil, a: (A, A)): HNil = HNil

//       def gradA(p: HNil, a: (A, A)): (A, A) = (MultiplicativeMonoid[A].one, MultiplicativeMonoid[A].one)
//     }
//   )
  
  implicit class Ops[P, A, C, GP, GA](idag: DiffPDag[P, A, C, GP, GA]) {
    def ||[Q, B, D, GP2, GA2](other: DiffPDag[Q, B, D, GP2, GA2])(
      implicit mergePQ: Merge[P, Q], mergeGradPQ: Merge[GP, GP2]
    ): DiffPDag[mergePQ.PQ, (A, B), (C, D), mergeGradPQ.PQ, (GA, GA2)] = par(idag, other)

    def >>>[Q, B, GQ, GB](other: DiffPDag[Q, C, B, GQ, GB])(
      implicit mergerPQ: Merge[P, Q], mergerGradPQ: Merge[GP, GQ]
    ): DiffPDag[mergerPQ.PQ, A, B, mergerGradPQ.PQ, (GA, GB)] = compose(other, idag)
  }

//   def f[A:MultiplicativeMonoid] = (id[A] || id[A]) >>> (id[A] || id[A])

//   // implicit val i = new MultiplicativeMonoid[Int] {
//   //   def one = 1
//   //   def times(a: Int, b: Int) = a * b
//   // }
//   // f[Int].apply(HNil, (5, 5))

  def Sigmoid[R, D <: Dim2[_, _]]: DiffPDag[
    HNil, Mat[R, D], Mat[R, D], HNil, Mat[R, D]
  ] = BasicDiffPDag[HNil, Mat[R, D], Mat[R, D], HNil, Mat[R, D]](
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

  def Dense0[S <: Singleton, R, InR <: XInt, OutR <: XInt, OutC <: XInt]: DiffPDag[
      FieldType[S, Mat[R, OutR x InR]] :: HNil
    , Mat[R, InR x OutC]
    , Mat[R, OutR x OutC]
    , FieldType[S, Mat[R, InR x OutC]] :: HNil
    , Mat[R, OutR x InR]
  ] = 
    BasicDiffPDag(
      new ParametrisedDiff[
        FieldType[S, Mat[R, OutR x InR]] :: HNil
      , Mat[R, InR x OutC]
      , Mat[R, OutR x OutC]
      , FieldType[S, Mat[R, InR x OutC]] :: HNil
      , Mat[R, OutR x InR]
      ] {
        def apply(weights: FieldType[S, Mat[R, OutR x InR]] :: HNil, in: Mat[R, InR x OutC]): Mat[R, OutR x OutC] = {
          new Mat[R, OutR x OutC](weights.head.value.mmul(in.value))
        }

        def gradP(weights: FieldType[S, Mat[R, OutR x InR]] :: HNil, in: Mat[R, InR x OutC]): FieldType[S, Mat[R, InR x OutC]] :: HNil = {
          shapeless.labelled.field[S](in) :: HNil
        }

        def gradA(weights: FieldType[S, Mat[R, OutR x InR]] :: HNil, in: Mat[R, InR x OutC]): Mat[R, OutR x InR] = {
          weights.head
        }
      }
    )

  def Dense[S <: Singleton, R, In <: Dim2[_, _], Out <: Dim2[_, _]](
    implicit same: Dim2SameCol[In, Out]
  ): DiffPDag[
      FieldType[S, Mat[R, same.OutR x same.In]] :: HNil
    , Mat[R, same.In x same.OutC]
    , Mat[R, same.OutR x same.OutC]
    , FieldType[S, Mat[R, same.In x same.OutC]] :: HNil
    , Mat[R, same.OutR x same.In]
  ] = Dense0[S, R, same.In, same.OutR, same.OutC]

  def nnetlayer[R, InR <: XInt, OutR <: XInt, OutC <: XInt] = 
    Dense["s", R, InR x OutC, OutR x OutC] >>> Sigmoid // || Dense[R, InR x OutC, OutR x OutC]("t") 

}
