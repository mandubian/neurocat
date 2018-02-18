package neurocat
package idag9

import shapeless.{HList, HNil, ::, DepFn2}
import cats.Monoid

import algebra.ring.{MultiplicativeMonoid, AdditiveSemigroup}

import shapeless.labelled._
import shapeless._ ; import syntax.singleton._ ; import record._

import cats.evidence._

import singleton.ops._
import org.nd4j.linalg.factory.Nd4j
import nd4j._
import typeclasses._

sealed trait Valued[+A]

// case class IdV[A]() extends Valued[A] {
//   def value(implicit d: DiagOned[A]): A = d.diagOne
// }
// case class ZeroV[A]() extends Valued[A] {
//   def value(implicit d: Zeroed[A]): A = d.zero
// }

// case class V[A](a: A) extends Valued[A] {
//   def value: A = a
// }

// trait NoneV extends Valued[Nothing]
// case object NoneV extends NoneV


trait DagAlgebra[Ctx, Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out], Out[p, a, b]]
  extends Dag.Id.Algebra[Ctx, Alg, Out]
  with Dag.ProdDiffDag.Algebra[Ctx, Alg, Out]
  with Dag.ProdDag.Algebra[Ctx, Alg, Out]
  with Dag.ComposeDiffDag.Algebra[Ctx, Alg, Out] 
  with Dag.ComposeDag.Algebra[Ctx, Alg, Out] 
  with Dag.FstDag.Algebra[Ctx, Alg, Out]
  with Dag.SndDag.Algebra[Ctx, Alg, Out]
  with Dag.ConstDag.Algebra[Ctx, Alg, Out]
  with Dag.SplitDagDiff.Algebra[Ctx, Alg, Out]
  with Dag.SplitDag.Algebra[Ctx, Alg, Out]
  with Dag.JoinDag.Algebra[Ctx, Alg, Out]
  with Dag.MergerDag.Algebra[Ctx, Alg, Out]
{
  self =>
  import Dag._
    
  // def noneV[P, A]: Out[P, A, NoneV]

}

trait Dag[Ctx, P, A, B, Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]] {

  def compile[Out[p, a, b]](
    compiler: Alg[Out]
  ): Out[P, A, B]

}

trait DiffDag[Ctx, P, A, B, Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]] extends Dag[Ctx, P, A, B, Alg] {

  // type GradP <: Dag[Ctx, P, (A, B), P, Alg]
  def gradP: Dag[Ctx, P, (A, B), P, Alg]

  // type GradA <: Dag[Ctx, P, (A, B), A, Alg]
  def gradA: Dag[Ctx, P, (A, B), A, Alg]

}



object Dag {
  implicit def amab[A : AdditiveSemigroup, B : AdditiveSemigroup] =
    new AdditiveSemigroup[(A, B)] {
      def plus(x: (A, B), y: (A, B)): (A, B) = (AdditiveSemigroup[A].plus(x._1, y._1), AdditiveSemigroup[B].plus(x._2, y._2))
      // def zero: (A, B) = (AdditiveSemigroup[A].zero, AdditiveSemigroup[B].zero)
    }


  case class Id[Ctx, A, Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]]()
  extends DiffDag[Ctx, HNil, A, A, Alg] {
    self =>

    def compile[Out[p, a, b]](
      compiler: Alg[Out]
    ): Out[HNil, A, A] = compiler.compile(self)

    def gradA: Dag[Ctx, HNil, (A, A), A, Alg] = new Dag[Ctx, HNil, (A, A), A, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[HNil, (A, A), A] = compiler.grada(self)
    }

    def gradP: Dag[Ctx, HNil, (A, A), HNil, Alg] = new Dag[Ctx, HNil, (A, A), HNil, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[HNil, (A, A), HNil] = compiler.gradp(self)
    }

  }


  object Id {

    trait Algebra[Ctx, Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out], Out[p, a, b]] {
      self: DagAlgebra[Ctx, Alg, Out] =>
        
      def compile[A](
        dag: Id[Ctx, A, Alg]
      ): Out[HNil, A, A]

      def grada[A](
        g: Id[Ctx, A, Alg]
      ): Out[HNil, (A, A), A]

      def gradp[A](
        g: Id[Ctx, A, Alg]
      ): Out[HNil, (A, A), HNil]
    }

  }

  abstract class ProdDag[
    Ctx
  , P, PA, PB
  , Q, QB
  , PQ
  , Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]
  ] private[neurocat] (
    val f: Dag[Ctx, P, PA, PB, Alg]
  , val g: Dag[Ctx, Q, PA, QB, Alg]
  ) extends Dag[Ctx, PQ, PA, (PB, QB), Alg] {
    self =>
    def merger: Merger.Aux[P, Q, PQ]

    def compile[Out[p, a, b]](
      compiler: Alg[Out]
    ): Out[PQ, PA, (PB, QB)] = compiler.compile(this)

  }

  object ProdDag {
    trait Algebra[Ctx, Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out], Out[p, a, b]] {
      self: DagAlgebra[Ctx, Alg, Out] =>
        
      def compile[
        P, PA, PB
      , Q, QB
      , PQ
      ](
        dag: ProdDag[Ctx, P, PA, PB, Q, QB, PQ, Alg]
      ): Out[PQ, PA, (PB, QB)]

    }
  }

  abstract class ProdDiffDag[
    Ctx
  , P, PA : AdditiveSemigroup, PB
  , Q, QB
  , PQ
  , Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]
  ] private[neurocat] (
    val f: DiffDag[Ctx, P, PA, PB, Alg]
  , val g: DiffDag[Ctx, Q, PA, QB, Alg]
  ) extends DiffDag[Ctx, PQ, PA, (PB, QB), Alg] {
    self =>
    def merger: Merger.Aux[P, Q, PQ]

    def compile[Out[p, a, b]](
      compiler: Alg[Out]
    ): Out[PQ, PA, (PB, QB)] = compiler.compile(this)

    def gradA: Dag[Ctx, PQ, (PA, (PB, QB)), PA, Alg] = new Dag[Ctx, PQ, (PA, (PB, QB)), PA, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[PQ, (PA, (PB, QB)), PA] = compiler.grada(self)
    }

    def gradP: Dag[Ctx, PQ, (PA, (PB, QB)), PQ, Alg] = new Dag[Ctx, PQ, (PA, (PB, QB)), PQ, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[PQ, (PA, (PB, QB)), PQ] = compiler.gradp(self)
    }    
  }

  object ProdDiffDag {
    trait Algebra[Ctx, Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out], Out[p, a, b]] {
      self: DagAlgebra[Ctx, Alg, Out] =>
        
      def compile[
        P, PA : AdditiveSemigroup, PB
      , Q, QB
      , PQ
      ](
        dag: ProdDiffDag[Ctx, P, PA, PB, Q, QB, PQ, Alg]
      ): Out[PQ, PA, (PB, QB)]

      def grada[
        P, PA : AdditiveSemigroup, PB
      , Q, QB
      , PQ
      ](
        dag: ProdDiffDag[Ctx, P, PA, PB, Q, QB, PQ, Alg]
      ): Out[PQ, (PA, (PB, QB)), PA]

      def gradp[
        P, PA : AdditiveSemigroup, PB
      , Q, QB
      , PQ
      ](
        dag: ProdDiffDag[Ctx, P, PA, PB, Q, QB, PQ, Alg]
      ): Out[PQ, (PA, (PB, QB)), PQ]
    }
  }


  abstract class ComposeDag[
    Ctx
  , P, PA, PB
  , Q, QC
  , PQ
  , Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]
  ] private[neurocat] (
    val g: Dag[Ctx, Q, PB, QC, Alg]
  , val f: Dag[Ctx, P, PA, PB, Alg]
  ) extends Dag[Ctx, PQ, PA, QC, Alg] {
    self =>
    def merger: Merger.Aux[P, Q, PQ]

    def compile[Out[p, a, b]](
      compiler: Alg[Out]
    ): Out[PQ, PA, QC] = compiler.compile(this)

  }


  object ComposeDag {
    trait Algebra[Ctx, Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out], Out[p, a, b]] {
      self: DagAlgebra[Ctx, Alg, Out] =>
      
      def compile[
        P, PA, PB
      , Q, QC
      , PQ
      ](
        dag: ComposeDag[Ctx, P, PA, PB, Q, QC, PQ, Alg]
      ): Out[PQ, PA, QC]

    }
  }

  abstract class ComposeDiffDag[
    Ctx
  , P, PA, PB
  , Q, QC
  , PQ
  , Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]
  ] private[neurocat] (
    val g: DiffDag[Ctx, Q, PB, QC, Alg]
  , val f: DiffDag[Ctx, P, PA, PB, Alg]
  ) extends DiffDag[Ctx, PQ, PA, QC, Alg] {
    self =>
    def merger: Merger.Aux[P, Q, PQ]

    def compile[Out[p, a, b]](
      compiler: Alg[Out]
    ): Out[PQ, PA, QC] = compiler.compile(this)

    def gradA = new Dag[Ctx, PQ, (PA, QC), PA, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[PQ, (PA, QC), PA] = compiler.grada(self)
    }

    def gradP = new Dag[Ctx, PQ, (PA, QC), PQ, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[PQ, (PA, QC), PQ] = compiler.gradp(self)
    }
  }

  object ComposeDiffDag {
    trait Algebra[Ctx, Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out], Out[p, a, b]] {
      self: DagAlgebra[Ctx, Alg, Out] =>
      
      def compile[
        P, PA, PB
      , Q, QC
      , PQ
      ](
        dag: ComposeDiffDag[Ctx, P, PA, PB, Q, QC, PQ, Alg]
      ): Out[PQ, PA, QC]

      def grada[
        P, PA, PB
      , Q, QC
      , PQ
      ](
        dag: ComposeDiffDag[Ctx, P, PA, PB, Q, QC, PQ, Alg]
      ): Out[PQ, (PA, QC), PA]

      def gradp[
        P, PA, PB
      , Q, QC
      , PQ
      ](
        dag: ComposeDiffDag[Ctx, P, PA, PB, Q, QC, PQ, Alg]
      ): Out[PQ, (PA, QC), PQ]
    }
  }

  case class FstDag[
    Ctx
  , A, B
  , Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]
  ]() extends DiffDag[
    Ctx, HNil, (A, B), A, Alg
  ] {
    self =>

    def compile[Out[p, a, b]](
      compiler: Alg[Out]
    ): Out[HNil, (A, B), A] = compiler.compile(this)

    def gradA: Dag[Ctx, HNil, ((A, B), A), (A, B), Alg] =
      new Dag[Ctx, HNil, ((A, B), A), (A, B), Alg] {
        def compile[Out[p, a, b]](
          compiler: Alg[Out]
        ): Out[HNil, ((A, B), A), (A, B)] = compiler.grada(self)
      }

    def gradP: Dag[Ctx, HNil, ((A, B), A), HNil, Alg] =
      new Dag[Ctx, HNil, ((A, B), A), HNil, Alg] {
        def compile[Out[p, a, b]](
          compiler: Alg[Out]
        ): Out[HNil, ((A, B), A), HNil] = compiler.gradp(self)
      }

  }

  object FstDag {

    trait Algebra[Ctx, Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out], Out[p, a, b]] {
      self: DagAlgebra[Ctx, Alg, Out] =>

      def compile[A, B](
        dag: FstDag[Ctx, A, B, Alg]
      ): Out[HNil, (A, B), A]

      def grada[A, B](
        g: FstDag[Ctx, A, B, Alg]
      ): Out[HNil, ((A, B), A), (A, B)]

      def gradp[A, B](
        g: FstDag[Ctx, A, B, Alg]
      ): Out[HNil, ((A, B), A), HNil]
    }
  }

  case class SndDag[
    Ctx
  , A, B
  , Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]
  ]() extends DiffDag[
    Ctx, HNil, (A, B), B, Alg
  ] {
    self =>

    def compile[Out[p, a, b]](
      compiler: Alg[Out]
    ): Out[HNil, (A, B), B] = compiler.compile(this)

    def gradA: Dag[Ctx, HNil, ((A, B), B), (A, B), Alg] = new Dag[Ctx, HNil, ((A, B), B), (A, B), Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[HNil, ((A, B), B), (A, B)] = compiler.grada(self)
    }

    def gradP: Dag[Ctx, HNil, ((A, B), B), HNil, Alg] = new Dag[Ctx, HNil, ((A, B), B), HNil, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[HNil, ((A, B), B), HNil] = compiler.gradp(self)
    }

  }

  object SndDag {

    trait Algebra[Ctx, Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out], Out[p, a, b]] {
      self: DagAlgebra[Ctx, Alg, Out] =>

      def compile[A, B](
        dag: SndDag[Ctx, A, B, Alg]
      ): Out[HNil, (A, B), B]

      def grada[A, B](
        g: SndDag[Ctx, A, B, Alg]
      ): Out[HNil, ((A, B), B), (A, B)]

      def gradp[A, B](
        g: SndDag[Ctx, A, B, Alg]
      ): Out[HNil, ((A, B), B), HNil]
    }
  }


  case class ConstDag[
    Ctx
  , A
  , Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]
  ](a: () => A) extends DiffDag[
    Ctx, HNil, Unit, A, Alg
  ] {
    self =>

    def compile[Out[p, a, b]](
      compiler: Alg[Out]
    ): Out[HNil, Unit, A] = compiler.compile(self)

    def gradA = new Dag[Ctx, HNil, (Unit, A), Unit, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[HNil, (Unit, A), Unit] = compiler.grada(self)
    }

    def gradP = new Dag[Ctx, HNil, (Unit, A), HNil, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[HNil, (Unit, A), HNil] = compiler.gradp(self)
    }

  }  

  object ConstDag {

    trait Algebra[Ctx, Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out], Out[p, a, b]] {
      self: DagAlgebra[Ctx, Alg, Out] =>

      def compile[A, B](
        dag: ConstDag[Ctx, A, Alg]
      ): Out[HNil, Unit, A]

      def grada[A, B](
        g: ConstDag[Ctx, A, Alg]
      ): Out[HNil, (Unit, A), Unit]

      def gradp[A, B](
        g: ConstDag[Ctx, A, Alg]
      ): Out[HNil, (Unit, A), HNil]
    }
  }


  case class SplitDagDiff[
    Ctx
  , A : Comonoid : AdditiveSemigroup
  , Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]
  ]() extends DiffDag[Ctx, HNil, A, (A, A), Alg] {
    self =>
    def compile[Out[p, a, b]](
      compiler: Alg[Out]
    ): Out[HNil, A, (A, A)] = compiler.compile(this)

    def gradA = new Dag[Ctx, HNil, (A, (A, A)), A, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[HNil, (A, (A, A)), A] = compiler.grada(self)
    }

    def gradP = new Dag[Ctx, HNil, (A, (A, A)), HNil, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[HNil, (A, (A, A)), HNil] = compiler.gradp(self)
    }
  }

  object SplitDagDiff {

    trait Algebra[Ctx, Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out], Out[p, a, b]] {
      self: DagAlgebra[Ctx, Alg, Out] =>

      def compile[A : Comonoid](
        dag: SplitDagDiff[Ctx, A, Alg]
      ): Out[HNil, A, (A, A)]

      def grada[A : AdditiveSemigroup](
        g: SplitDagDiff[Ctx, A, Alg]
      ): Out[HNil, (A, (A, A)), A]

      def gradp[A](
        g: SplitDagDiff[Ctx, A, Alg]
      ): Out[HNil, (A, (A, A)), HNil]
    }
  }

  case class SplitDag[
    Ctx
  , A : Comonoid
  , Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]
  ]() extends Dag[Ctx, HNil, A, (A, A), Alg] {
    self =>
    def compile[Out[p, a, b]](
      compiler: Alg[Out]
    ): Out[HNil, A, (A, A)] = compiler.compile(this)

  }

  object SplitDag {

    trait Algebra[Ctx, Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out], Out[p, a, b]] {
      self: DagAlgebra[Ctx, Alg, Out] =>

      def compile[A : Comonoid](
        dag: SplitDag[Ctx, A, Alg]
      ): Out[HNil, A, (A, A)]
    }
  }

  case class JoinDag[
    Ctx
  , A : AdditiveSemigroup
  , Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]
  ]() extends Dag[Ctx, HNil, (A, A), A, Alg] {
    self =>
    def compile[Out[p, a, b]](
      compiler: Alg[Out]
    ): Out[HNil, (A, A), A] = compiler.compile(self)

  }

  object JoinDag {

    trait Algebra[Ctx, Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out], Out[p, a, b]] {
      self: DagAlgebra[Ctx, Alg, Out] =>

      def compile[A : AdditiveSemigroup](
        dag: JoinDag[Ctx, A, Alg]
      ): Out[HNil, (A, A), A]
    }
  }

  abstract class MergerDag[
    Ctx
  , P, Q
  , PQ
  , Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]
  ]() extends Dag[Ctx, HNil, (P, Q), PQ, Alg] {
    self =>

    def merger: Merger.Aux[P, Q, PQ]

    def compile[Out[p, a, b]](
      compiler: Alg[Out]
    ): Out[HNil, (P, Q), PQ] = compiler.compile(self)

  }

  object MergerDag {

    trait Algebra[Ctx, Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out], Out[p, a, b]] {
      self: DagAlgebra[Ctx, Alg, Out] =>

      def compile[P, Q, PQ](
        dag: MergerDag[Ctx, P, Q, PQ, Alg]
      ): Out[HNil, (P, Q), PQ]
    }
  }
}

trait DagDsl0 {
  import Dag._

  def id0[Ctx, A, Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]]: DiffDag[Ctx, HNil, A, A, Alg] = Id()

  def prod0[
    Ctx
  , P, PA, PB
  , Q, QB, QGP
  , PQ
  , Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]
  ](
    f: Dag[Ctx, P, PA, PB, Alg]
  , g: Dag[Ctx, Q, PA, QB, Alg]
  )(
    implicit merger0: Merger[P, Q]
  ): Dag[Ctx, merger0.Out, PA, (PB, QB), Alg]
  = new ProdDag[
      Ctx
    , P, PA, PB
    , Q, QB
    , merger0.Out
    , Alg
    ](f, g) {
      def merger = merger0
    }

  def prodDiff0[
    Ctx
  , P, PA : AdditiveSemigroup, PB
  , Q, QB, QGP
  , PQ
  , Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]
  ](
    f: DiffDag[Ctx, P, PA, PB, Alg]
  , g: DiffDag[Ctx, Q, PA, QB, Alg]
  )(
    implicit merger0: Merger[P, Q]
  ): DiffDag[Ctx, merger0.Out, PA, (PB, QB), Alg]
  = new ProdDiffDag[
      Ctx
    , P, PA, PB
    , Q, QB
    , merger0.Out
    , Alg
    ](f, g) {
      def merger = merger0
    }

  def fst0[
    Ctx, A, B, Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]
  ](): DiffDag[Ctx, HNil, (A, B), A, Alg] = FstDag()

  def snd0[
    Ctx, A, B, Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]
  ](): DiffDag[Ctx, HNil, (A, B), B, Alg] = SndDag()

  def splitDiff0[
    Ctx, A : Comonoid : AdditiveSemigroup, Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]
  ](): DiffDag[Ctx, HNil, A, (A, A), Alg] = SplitDagDiff()

  def split0[
    Ctx, A : Comonoid, Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]
  ](): Dag[Ctx, HNil, A, (A, A), Alg] = SplitDag()

  def compose0[
    Ctx
  , P, A, B, PGP, PGA
  , Q, C, QGP, QGA
  , Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]
  ](g: Dag[Ctx, Q, B, C, Alg], f: Dag[Ctx, P, A, B, Alg])(
    implicit merger0: Merger[P, Q]
  ): Dag[Ctx, merger0.Out, A, C, Alg]
  = new ComposeDag[
      Ctx
    , P, A, B
    , Q, C
    , merger0.Out
    , Alg
    ](g, f) {
      def merger = merger0
    }

  def composeDiff0[
    Ctx
  , P, A, B, PGP, PGA
  , Q, C, QGP, QGA
  , Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]
  ](g: DiffDag[Ctx, Q, B, C, Alg], f: DiffDag[Ctx, P, A, B, Alg])(
    implicit merger0: Merger[P, Q]
  ): DiffDag[Ctx, merger0.Out, A, C, Alg]
  = new ComposeDiffDag[
      Ctx
    , P, A, B
    , Q, C
    , merger0.Out
    , Alg
    ](g, f) {
      def merger = merger0
    }

  def par0[
    Ctx
  , P, PA, PB
  , Q, QA, QB
  , Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]
  ](f: Dag[Ctx, P, PA, PB, Alg], g: Dag[Ctx, Q, QA, QB, Alg])(
    implicit merger0: Merger[P, Q]
  ): Dag[Ctx, merger0.Out, (PA, QA), (PB, QB), Alg] =
    prod0(
      compose0(f, fst0[Ctx, PA, QA, Alg])
    , compose0(g, snd0[Ctx, PA, QA, Alg])
    )

  def parDiff0[
    Ctx
  , P, PA : AdditiveSemigroup, PB
  , Q, QA : AdditiveSemigroup, QB
  , Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]
  ](f: DiffDag[Ctx, P, PA, PB, Alg], g: DiffDag[Ctx, Q, QA, QB, Alg])(
    implicit merger0: Merger[P, Q]
  ): DiffDag[Ctx, merger0.Out, (PA, QA), (PB, QB), Alg] =
    prodDiff0(
      composeDiff0(f, fst0[Ctx, PA, QA, Alg])
    , composeDiff0(g, snd0[Ctx, PA, QA, Alg])
    )

  def assocRDiff0[
    Ctx
  , A : AdditiveSemigroup, B : AdditiveSemigroup, C : AdditiveSemigroup
  , Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]
  ]: DiffDag[Ctx, HNil, ((A, B), C), (A, (B, C)), Alg] = {
    val pa = composeDiff0(fst0[Ctx, A, B, Alg], fst0[Ctx, (A, B), C, Alg])
    val pb = composeDiff0(snd0[Ctx, A, B, Alg], fst0[Ctx, (A, B), C, Alg])
    val pc = snd0[Ctx, (A, B), C, Alg]
    prodDiff0(pa, prodDiff0(pb, pc))
  }

  def assocR0[
    Ctx
  , A, B, C
  , Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]
  ]: Dag[Ctx, HNil, ((A, B), C), (A, (B, C)), Alg] = {
    val pa = compose0(fst0[Ctx, A, B, Alg], fst0[Ctx, (A, B), C, Alg])
    val pb = compose0(snd0[Ctx, A, B, Alg], fst0[Ctx, (A, B), C, Alg])
    val pc = snd0[Ctx, (A, B), C, Alg]
    prod0(pa, prod0(pb, pc))
  }  
}


trait DagDsl[Ctx, Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]] extends DagDsl0 {
  import Dag._

  def id[A]: DiffDag[Ctx, HNil, A, A, Alg] = Id()

  def prod[
      P, PA, PB
    , Q, QB, QGP
    , PQ
  ](
    f: Dag[Ctx, P, PA, PB, Alg]
  , g: Dag[Ctx, Q, PA, QB, Alg]
  )(
    implicit merger0: Merger[P, Q]
  ): Dag[Ctx, merger0.Out, PA, (PB, QB), Alg]
  = new ProdDag[
      Ctx
    , P, PA, PB
    , Q, QB
    , merger0.Out
    , Alg
    ](f, g) {
      def merger = merger0
    }

  def prodDiff[
      P, PA : AdditiveSemigroup, PB
    , Q, QB, QGP
    , PQ
  ](
    f: DiffDag[Ctx, P, PA, PB, Alg]
  , g: DiffDag[Ctx, Q, PA, QB, Alg]
  )(
    implicit merger0: Merger[P, Q]
  ): DiffDag[Ctx, merger0.Out, PA, (PB, QB), Alg]
  = new ProdDiffDag[
      Ctx
    , P, PA, PB
    , Q, QB
    , merger0.Out
    , Alg
    ](f, g) {
      def merger = merger0
    }

  def fst[
    A, B
  ](): DiffDag[Ctx, HNil, (A, B), A, Alg] = FstDag()

  def snd[
    A, B
  ](): DiffDag[Ctx, HNil, (A, B), B, Alg] = SndDag()

  def splitDiff[
    A : Comonoid : AdditiveSemigroup
  ](): DiffDag[Ctx, HNil, A, (A, A), Alg] = SplitDagDiff()

  def split[
    A : Comonoid
  ](): Dag[Ctx, HNil, A, (A, A), Alg] = SplitDag()
  
  def join[
    A : AdditiveSemigroup
  ](): Dag[Ctx, HNil, (A, A), A, Alg] = JoinDag()

  def merge[
    P, Q
  ](implicit merger0: Merger[P, Q]): Dag[Ctx, HNil, (P, Q), merger0.Out, Alg] = new MergerDag[Ctx, P, Q, merger0.Out, Alg] {
    def merger = merger0
  }

  def compose[
    P, A, B, PGP, PGA
  , Q, C, QGP, QGA
  ](g: Dag[Ctx, Q, B, C, Alg], f: Dag[Ctx, P, A, B, Alg])(
    implicit merger0: Merger[P, Q]
  ): Dag[Ctx, merger0.Out, A, C, Alg]
  = new ComposeDag[
      Ctx
    , P, A, B
    , Q, C
    , merger0.Out
    , Alg
    ](g, f) {
      def merger = merger0
    }

  def composeDiff[
    P, A, B, PGP, PGA
  , Q, C, QGP, QGA
  ](g: DiffDag[Ctx, Q, B, C, Alg], f: DiffDag[Ctx, P, A, B, Alg])(
    implicit merger0: Merger[P, Q], mergerg0: Merger[PGP, QGP]
  ): DiffDag[Ctx, merger0.Out, A, C, Alg]
  = new ComposeDiffDag[
      Ctx
    , P, A, B
    , Q, C
    , merger0.Out
    , Alg
    ](g, f) {
      def merger = merger0
      def mergerg = mergerg0
    }

  def par[
    P, PA, PB
  , Q, QA, QB
  ](f: Dag[Ctx, P, PA, PB, Alg], g: Dag[Ctx, Q, QA, QB, Alg])(
    implicit merger0: Merger[P, Q]
  ): Dag[Ctx, merger0.Out, (PA, QA), (PB, QB), Alg] =
    prod(
      compose(f, fst[PA, QA])
    , compose(g, snd[PA, QA])
    )

  def parDiff[
    P, PA : AdditiveSemigroup, PB
  , Q, QA : AdditiveSemigroup, QB
  ](f: DiffDag[Ctx, P, PA, PB, Alg], g: DiffDag[Ctx, Q, QA, QB, Alg])(
    implicit merger0: Merger[P, Q]
  ): DiffDag[Ctx, merger0.Out, (PA, QA), (PB, QB), Alg] =
    prodDiff(
      composeDiff(f, fst[PA, QA])
    , composeDiff(g, snd[PA, QA])
    )

  def assocRDiff[
    A : AdditiveSemigroup, B : AdditiveSemigroup, C : AdditiveSemigroup
  ]: DiffDag[Ctx, HNil, ((A, B), C), (A, (B, C)), Alg] = {
    val pa = composeDiff(fst[A, B], fst[(A, B), C])
    val pb = composeDiff(snd[A, B], fst[(A, B), C])
    val pc = snd[(A, B), C]
    prodDiff(pa, prodDiff(pb, pc))
  }

  def assocR[
    A, B, C
  ]: Dag[Ctx, HNil, ((A, B), C), (A, (B, C)), Alg] = assocR0[Ctx, A, B, C, Alg]
}


trait ParametrisedFunction[P, A, B] extends Function2[P, A, B]

trait ParametrisedDiff[P, A, B] extends ParametrisedFunction[P, A, B] {
  type GP
  def gradP(p: P, a: A): GP

  type GA
  def gradA(p: P, a: A): GA
}

object ParametrisedDiff {
  type Aux[P, A, B, GP0, GA0] = ParametrisedDiff[P, A, B] { type GP = GP0; type GA = GA0 }

  def apply[P, A, B](f: (P, A) => B) = new ParametrisedDiff[P, A, B] {
    def apply(p: P, a: A): B = f(p, a)
    type GP = HNil
    def gradP(p: P, a: A): HNil = HNil
    type GA = HNil
    def gradA(p: P, a: A): HNil = HNil
  }
}


trait CostFn[Ctx, Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]] {
  def apply[A]: Dag[Ctx, HNil, (A, A), A, Alg]
}

trait Learn[
  Ctx
, P, A, B
, Alg[out[p, a, b]] <: DagAlgebra[Ctx, Alg, out]
] {
  def implement: Dag[Ctx, P, A, B, Alg]

  def update[S](
        eps: S
      // , costDiff: Dag[Ctx, HNil, (B, B), B, Alg]
      , costDiff: CostFn[Ctx, Alg]
      , costDiffInvert: CostFn[Ctx, Alg]      
      ): Dag[Ctx, P, (A, B), P, Alg]
  
  def request[S](
        eps: S
      // , costDiff: Dag[Ctx, HNil, (B, B), B, Alg]
      // , costDiffInvert: Dag[Ctx, HNil, (A, A), A, Alg]
      , costDiff: CostFn[Ctx, Alg]
      , costDiffInvert: CostFn[Ctx, Alg]      
      ): Dag[Ctx, P, (A, B), A, Alg]
}

object Learn {
  import Dag._


  trait LearnAlgebra[Ctx, Out[p, a, b]]
    extends DagAlgebra[Ctx, Lambda[out[p, a, b] => LearnAlgebra[Ctx, out]], Out]

  class LearnCompiler[
    Ctx
  ](
    dsl0: DagDsl[Ctx, Lambda[out[p, a, b] => LearnAlgebra[Ctx, out]]]
  ) extends LearnAlgebra[Ctx, Lambda[(p, a, b) => Learn[Ctx, p, a, b, Lambda[out[p, a, b] => LearnAlgebra[Ctx, out]]]]] {
    self =>

    type LA[out[p, a, b]] = LearnAlgebra[Ctx, out]
    type Out[p, a, b] = Learn[Ctx, p, a, b, Lambda[out[p, a, b] => LearnAlgebra[Ctx, out]]]

    def compile[
      A
    ](g: Id[Ctx, A, LA]): Learn[Ctx, HNil, A, A, LA] = new Learn[
        Ctx
      , HNil, A, A
      , LA
      ] {
        implicit def aCo[A] = new Comonoid[A] {
          def split(a: A): (A, A) = (a, a)
          def destroy(a: A) = ()
        }
        def implement: Dag[Ctx, HNil, A, A, LA] = g

        def update[S](
          eps: S
        // , costDiff: Dag[Ctx, HNil, (A, A), A, LA]
        , costDiff: CostFn[Ctx, LA]
        , costDiffInvert: CostFn[Ctx, LA]      
        ): Dag[Ctx, HNil, (A, A), HNil, LA] = g.gradP

        def request[S](
          eps: S
        // , costDiff: Dag[Ctx, HNil, (A, A), A, LA]
        // , costDiffInvert: Dag[Ctx, HNil, (A, A), A, LA]
        , costDiff: CostFn[Ctx, LA]
        , costDiffInvert: CostFn[Ctx, LA]      
        ): Dag[Ctx, HNil, (A, A), A, LA] = {
          dsl0.snd[A, A]
        }
      }


    def compile[
      P, PA, PB
    , Q, QC
    , PQ
    ](
      dag: ComposeDiffDag[Ctx, P, PA, PB, Q, QC, PQ, LA]
    ): Learn[Ctx, PQ, PA, QC, LA] =
      new Learn[Ctx, PQ, PA, QC, LA] {
        implicit val m = dag.merger

        implicit def aCo = new Comonoid[PA] {
          def split(a: PA): (PA, PA) = (a, a)
          def destroy(a: PA) = ()
        }

        val pg = dag.g.compile[Out](self)
        val pf = dag.f.compile[Out](self)

        def implement: Dag[Ctx, PQ, PA, QC, LA] = {
          dsl0.compose(pg.implement, pf.implement)(dag.merger)
        }
        
        def update[S](
          eps: S
        , costDiff: CostFn[Ctx, LA]
        , costDiffInvert: CostFn[Ctx, LA]      
        ): Dag[Ctx, PQ, (PA, QC), PQ, LA] = {
          val idQC = dsl0.id[QC]
          val idPA = dsl0.id[PA]

          val pfu = pf.update(eps, costDiff, costDiffInvert)
          val pgu = pg.update(eps, costDiff, costDiffInvert)
          val pgr = pg.request(eps, costDiff, costDiffInvert)
          val pr = dsl0.par(
            pf.implement
          , idQC
          )
          val pgr2 = dsl0.compose(
            pgr
          , dsl0.par(
              pf.implement
            , idQC
            )
          )

          val u = dsl0.compose(
            dsl0.compose(pfu, dsl0.par(idPA, pgr2))
          , dsl0.compose(dsl0.assocR[PA, PA, QC], dsl0.par(dsl0.split[PA], idQC))
          )

          val v = dsl0.compose(
            pgu
          , dsl0.par(
              pf.implement
            , idQC
            )
          )

          dsl0.compose(dsl0.merge, dsl0.prod(u, v))
        }

        def request[S](
          eps: S
        , costDiff: CostFn[Ctx, LA]
        , costDiffInvert: CostFn[Ctx, LA]      
        ): Dag[Ctx, PQ, (PA, QC), PA, LA] = {
          val idQC = dsl0.id[QC]
          val idPA = dsl0.id[PA]

          val pfr = pf.request(eps, costDiff, costDiffInvert)
          val pgr = pg.request(eps, costDiff, costDiffInvert)
          val reqCompose = dsl0.compose(
              pgr
            , dsl0.par(
                pf.implement
              , idQC
              )
            )
          dsl0.compose(
            dsl0.compose(
              pfr
            , dsl0.par(idPA, reqCompose)
            )
          , dsl0.compose(dsl0.assocR[PA, PA, QC], dsl0.par(dsl0.split[PA], idQC))
          )
        }
      }

    def compile[A, B](
      dag: ConstDag[Ctx, A, LA]
    ): Learn[Ctx, HNil, Unit, A, LA] =
      compile0[HNil, Unit, A](dag)

    def compile0[
      P, A, B
    ](dag: DiffDag[Ctx, P, A, B, LA]): Learn[Ctx, P, A, B, LA] = new Learn[
        Ctx
      , P, A, B
      , LA
      ] {
        
        implicit def aCo[A] = new Comonoid[A] {
          def split(a: A): (A, A) = (a, a)
          def destroy(a: A) = ()
        }

        def implement: Dag[Ctx, P, A, B, LA] = dag

        def update[S](
          eps: S
        , costDiff: CostFn[Ctx, LA]
        , costDiffInvert: CostFn[Ctx, LA]
        ): Dag[Ctx, P, (A, B), P, LA] = {
          val idA = dsl0.id[A]
          val idB = dsl0.id[B]

          val err = dsl0.compose(
            costDiff[B]
          , dsl0.par(implement, idB)
          )
          val gradp = dsl0.compose(dag.gradP, dsl0.par(idA, err))

          val init = dsl0.compose(dsl0.assocR[A, A, B], dsl0.par(dsl0.split[A], idB))

          dsl0.compose(gradp, init)
        }

        def request[S](
          eps: S
        , costDiff: CostFn[Ctx, LA]
        , costDiffInvert: CostFn[Ctx, LA]
        ): Dag[Ctx, P, (A, B), A, LA] = {
          // for others, it will be the following
          val idA = dsl0.id[A]
          val idB = dsl0.id[B]

          val err = dsl0.compose(
            costDiff[B]
          , dsl0.par(implement, idB)
          )
          val grada = dsl0.compose(dag.gradA, dsl0.par(idA, err))
          // multiply by norm

          
          val r = dsl0.compose(costDiffInvert[A], dsl0.par(idA, grada))
          val r2 = dsl0.par(dsl0.split[A](), idB)
          val r3 = dsl0.compose(
                      r2
                    , dsl0.par(idA, idB)
                    )
          val r4 = dsl0.compose(
                        dsl0.assocR[A, A, B]
                      , r3
                      )
          val r5 = dsl0.prod(
            dsl0.fst[A, B]
          , r4
          )
          dsl0.compose(r, r5)
        }
      }

  }

}

object Test {
  trait ND4J

  trait Algebra[Out[p, a, b]]
    extends DagAlgebra[ND4J, Algebra, Out]

  val dsl = new DagDsl[ND4J, Algebra] {}
  import dsl._

  val idDag = id[Double]

  // implicit val im = Dag.Id.gradP[ND4J, Double, Algebra]

  // val i: Int = idDag.gradA
  // val idVrad = Dag.gradA(idDag)

  implicit val doubleAM = new AdditiveSemigroup[Double] {
    def plus(a: Double, b: Double) = a + b
  }

  val prd = prod(idDag, idDag)

  // object LearnAlg extends Learn.Algebra[ND4J, Algebra] {

  // }

  object ND4JAlgebra extends Algebra[ParametrisedFunction] {    
    self =>

    import Dag._

    // def noneV[
    //   P, A
    // ]: ParametrisedFunction[P, A, NoneV] = new ParametrisedFunction[P, A, NoneV] {
    //   def apply(p: P, a: A): NoneV = NoneV
    // }

//////////////////////////////////////////////////////////////////
// ID

    def compile[
      A
    ](g: Id[ND4J, A, Algebra]): ParametrisedFunction[HNil, A, A] = new ParametrisedFunction[HNil, A, A] {
      def apply(p: HNil, a: A): A = a
    }

    def grada[
      A
    ](
      dag: Id[ND4J, A, Algebra]
    ): ParametrisedFunction[HNil, (A, A), A] = new ParametrisedFunction[HNil, (A, A), A] {
      def apply(p: HNil, a: (A, A)): A = a._2
    }

    def gradp[
      A
    ](
      dag: Id[ND4J, A, Algebra]
    ): ParametrisedFunction[HNil, (A, A), HNil] = new ParametrisedFunction[HNil, (A, A), HNil] {
      def apply(p: HNil, a: (A, A)): HNil = HNil
    }

//////////////////////////////////////////////////////////////////
// PROD

    def compile[
        P, PA, PB
      , Q, QB
      , PQ
      ](
        dag: ProdDag[ND4J, P, PA, PB, Q, QB, PQ, Algebra]
      ): ParametrisedFunction[PQ, PA, (PB, QB)] =
        new ParametrisedFunction[PQ, PA, (PB, QB)] {
          val pf = dag.f.compile(self)
          val pg = dag.g.compile(self)

          def apply(pq: PQ, ab: PA): (PB, QB) = {
            ( pf(dag.merger.left(pq), ab)
            , pg(dag.merger.right(pq), ab)
            )
          }
        }

    def compile[
        P, PA : AdditiveSemigroup, PB
      , Q, QB
      , PQ
      ](
        dag: ProdDiffDag[ND4J, P, PA, PB, Q, QB, PQ, Algebra]
      ): ParametrisedFunction[PQ, PA, (PB, QB)] =
        new ParametrisedFunction[PQ, PA, (PB, QB)] {
          val pf = dag.f.compile(self)
          val pg = dag.g.compile(self)

          def apply(pq: PQ, ab: PA): (PB, QB) = {
            ( pf(dag.merger.left(pq), ab)
            , pg(dag.merger.right(pq), ab)
            )
          }
        }


    def grada[
      P, PA : AdditiveSemigroup, PB
    , Q, QB
    , PQ
    ](
      dag: ProdDiffDag[ND4J, P, PA, PB, Q, QB, PQ, Algebra]
    ): ParametrisedFunction[PQ, (PA, (PB, QB)), PA] = 
      new ParametrisedFunction[PQ, (PA, (PB, QB)), PA] {
        val pf = dag.f.gradA.compile(self)
        val pg = dag.g.gradA.compile(self)

        def apply(pq: PQ, ab: (PA, (PB, QB))): PA = {
          AdditiveSemigroup[PA].plus(
            pf(dag.merger.left(pq), (ab._1, ab._2._1))
          , pg(dag.merger.right(pq), (ab._1, ab._2._2))
        )
        }
      }

    def gradp[
      P, PA : AdditiveSemigroup, PB
    , Q, QB
    , PQ
    ](
      dag: ProdDiffDag[ND4J, P, PA, PB, Q, QB, PQ, Algebra]
    ): ParametrisedFunction[PQ, (PA, (PB, QB)), PQ] =
      new ParametrisedFunction[PQ, (PA, (PB, QB)), PQ] {
        val pf = dag.f.gradP.compile(self)
        val pg = dag.g.gradP.compile(self)

        def apply(pq: PQ, ab: (PA, (PB, QB))): PQ = {
          dag.merger(
            pf(dag.merger.left(pq), (ab._1, ab._2._1))
          , pg(dag.merger.right(pq), (ab._1, ab._2._2))
          )
        }
      }

//////////////////////////////////////////////////////////////////
// COMPOSE
    def compile[
      P, PA, PB
    , Q, QC
    , PQ
    ](
      dag: ComposeDag[ND4J, P, PA, PB, Q, QC, PQ, Algebra]
    ): ParametrisedFunction[PQ, PA, QC] =
      new ParametrisedFunction[PQ, PA, QC] {
        val pg = dag.g.compile(self)
        val pf = dag.f.compile(self)

        def apply(pq: PQ, ab: PA): QC = {
          pg(dag.merger.right(pq), pf(dag.merger.left(pq), ab))
        }
      }

    def compile[
      P, PA, PB
    , Q, QC
    , PQ
    ](
      dag: ComposeDiffDag[ND4J, P, PA, PB, Q, QC, PQ, Algebra]
    ): ParametrisedFunction[PQ, PA, QC] =
      new ParametrisedFunction[PQ, PA, QC] {
        val pg = dag.g.compile(self)
        val pf = dag.f.compile(self)

        def apply(pq: PQ, ab: PA): QC = {
          pg(dag.merger.right(pq), pf(dag.merger.left(pq), ab))
        }
      }

    def grada[
      P, PA, PB
    , Q, QC
    , PQ
    ](
      dag: ComposeDiffDag[ND4J, P, PA, PB, Q, QC, PQ, Algebra]
    ): ParametrisedFunction[PQ, (PA, QC), PA] =
      new ParametrisedFunction[PQ, (PA, QC), PA] {
        val pggrad = dag.g.gradA.compile(self)
        val pf = dag.f.compile(self)
        val pfgrad = dag.f.gradA.compile(self)

        def apply(pq: PQ, ab: (PA, QC)): PA = {
          val pb = pf(dag.merger.left(pq), ab._1)
          val pb2 = pggrad(dag.merger.right(pq), (pb, ab._2))
          pfgrad(dag.merger.left(pq), (ab._1, pb2))
        }
      }

    def gradp[
      P, PA, PB
    , Q, QC
    , PQ
    ](
      dag: ComposeDiffDag[ND4J, P, PA, PB, Q, QC, PQ, Algebra]
    ): ParametrisedFunction[PQ, (PA, QC), PQ] =
      new ParametrisedFunction[PQ, (PA, QC), PQ] {
        val pggrad = dag.g.gradP.compile(self)
        val pf = dag.f.compile(self)
        val pfgrad = dag.f.gradP.compile(self)

        def apply(pq: PQ, ab: (PA, QC)): PQ = {
          val p = dag.merger.left(pq)
          val q = dag.merger.right(pq)
          val (pa, qc) = ab
          val pb = pf(p, pa)
          val q2 = pggrad(q, (pb, qc))
          val p2 = pfgrad(p, (ab._1, pb))
          dag.merger(p2, q2)
        }
      }

//////////////////////////////////////////////////////////////////
// FST
    def compile[
      A, B
    ](
      dag: FstDag[ND4J, A, B, Algebra]
    ): ParametrisedFunction[HNil, (A, B), A] =
      new ParametrisedFunction[HNil, (A, B), A] {
        def apply(p: HNil, ab: (A, B)): A = {
          ab._1
        }
      }

    def grada[A, B](
      g: FstDag[ND4J, A, B, Algebra]
    ): ParametrisedFunction[HNil, ((A, B), A), (A, B)] =
      new ParametrisedFunction[HNil, ((A, B), A), (A, B)] {
        def apply(p: HNil, ab: ((A, B), A)): (A, B) = (ab._2, ab._1._2)
      }

    def gradp[A, B](
      g: FstDag[ND4J, A, B, Algebra]
    ): ParametrisedFunction[HNil, ((A, B), A), HNil] =
      new ParametrisedFunction[HNil, ((A, B), A), HNil] {
        def apply(p: HNil, ab: ((A, B), A)): HNil = HNil
      }


//////////////////////////////////////////////////////////////////
// SND
    def compile[
      A, B
    ](
      dag: SndDag[ND4J, A, B, Algebra]
    ): ParametrisedFunction[HNil, (A, B), B] =
      new ParametrisedFunction[HNil, (A, B), B] {
        def apply(p: HNil, ab: (A, B)): B = {
          ab._2
        }
      }

    def grada[A, B](
      g: SndDag[ND4J, A, B, Algebra]
    ): ParametrisedFunction[HNil, ((A, B), B), (A, B)] =
      new ParametrisedFunction[HNil, ((A, B), B), (A, B)] {
        def apply(p: HNil, ab: ((A, B), B)): (A, B) = (ab._1._1, ab._2)
      }

    def gradp[A, B](
      g: SndDag[ND4J, A, B, Algebra]
    ): ParametrisedFunction[HNil, ((A, B), B), HNil] =
      new ParametrisedFunction[HNil, ((A, B), B), HNil] {
        def apply(p: HNil, ab: ((A, B), B)): HNil = HNil
      }      

//////////////////////////////////////////////////////////////////
// CONST
    def compile[A, B](
      dag: ConstDag[ND4J, A, Algebra]
    ): ParametrisedFunction[HNil, Unit, A] =
      new ParametrisedFunction[HNil, Unit, A] {
        def apply(p: HNil, ab: Unit): A = dag.a()
      }

    def grada[A, B](
      g: ConstDag[ND4J, A, Algebra]
    ): ParametrisedFunction[HNil, (Unit, A), Unit] =
      new ParametrisedFunction[HNil, (Unit, A), Unit] {
        def apply(p: HNil, ab: (Unit, A)): Unit = ()
      }

    def gradp[A, B](
      g: ConstDag[ND4J, A, Algebra]
    ): ParametrisedFunction[HNil, (Unit, A), HNil] =
      new ParametrisedFunction[HNil, (Unit, A), HNil] {
        def apply(p: HNil, ab: (Unit, A)): HNil = HNil
      }

    def compile[A : Comonoid](
      dag: SplitDag[ND4J, A, Algebra]
    ): ParametrisedFunction[HNil, A, (A, A)] = new ParametrisedFunction[HNil, A, (A, A)] {
      def apply(p: HNil, ab: A): (A, A) = Comonoid[A].split(ab)
    }

    def compile[A : Comonoid](
      dag: SplitDagDiff[ND4J, A, Algebra]
    ): ParametrisedFunction[HNil, A, (A, A)] = new ParametrisedFunction[HNil, A, (A, A)] {
      def apply(p: HNil, ab: A): (A, A) = Comonoid[A].split(ab)
    }

    def grada[A : AdditiveSemigroup](
      g: SplitDagDiff[ND4J, A, Algebra]
    ): ParametrisedFunction[HNil, (A, (A, A)), A] = new ParametrisedFunction[HNil, (A, (A, A)), A] {
      def apply(p: HNil, ab: (A, (A, A))): A = AdditiveSemigroup[A].plus(ab._2._1, ab._2._2)
    }

    def gradp[A](
      g: SplitDagDiff[ND4J, A, Algebra]
    ): ParametrisedFunction[HNil, (A, (A, A)), HNil] = new ParametrisedFunction[HNil, (A, (A, A)), HNil] {
      def apply(p: HNil, ab: (A, (A, A))): HNil = HNil
    }

    def compile[P, Q, PQ](
      dag: MergerDag[ND4J, P, Q, PQ, Algebra]
    ): ParametrisedFunction[HNil, (P, Q), PQ] = new ParametrisedFunction[HNil, (P, Q), PQ] {
      def apply(p: HNil, pq: (P, Q)): PQ = dag.merger(pq._1, pq._2)
    }
    
    def compile[A : AdditiveSemigroup](
      dag: JoinDag[ND4J, A, Algebra]
    ):ParametrisedFunction[HNil, (A, A), A] = new ParametrisedFunction[HNil, (A, A), A] {
      def apply(p: HNil, pq: (A, A)): A = AdditiveSemigroup[A].plus(pq._1, pq._2)
    }
  }
  // val idVradF = idVrad.compile(ND4JAlgebra)
  val prdF = prd.compile(ND4JAlgebra)
  val (a, b) = prdF(HNil, 5)

  // implicit val d: DiagOned[Double] = ???


  // idVradF(HNil, 5)
}








