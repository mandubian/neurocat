package neurocat
package idag

import shapeless.HNil
import algebra.ring.AdditiveSemigroup
import typeclasses._


trait Split[A, B, C, S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out]]
extends Dag[HNil, A, (B, C), S, Alg] {
  self =>

  def merger: Merger.Aux[B, C, A]

  def compile[Out[p, a, b]](
    compiler: Alg[Out]
  ): Out[HNil, A, (B, C)] = compiler.compile(this)
}


object Split {

  abstract class Diff[
    A, B, C
  // algebra visitor
  , S, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ]()
  extends Split[A, B, C, S, Alg] with DiffDag[HNil, A, (B, C), S, Alg] {
    self =>

    def gradA = new Dag[HNil, (A, (B, C)), A, S, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[HNil, (A, (B, C)), A] = compiler.grada(self)
    }

    def gradP = new Dag[HNil, (A, (B, C)), HNil, S, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[HNil, (A, (B, C)), HNil] = compiler.gradp(self)
    }
  }

  trait Algebra[S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out], Out[p, a, b]] {      
    def compile[A, B, C](
      dag: Split[A, B, C, S, Alg]
    ): Out[HNil, A, (B, C)]
  }


  trait DiffAlgebra[
    S, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out], Out[p, a, b]
  ]
  extends Algebra[S, Alg, Out] {

    def grada[
      A, B, C
    ](
      g: Split.Diff[A, B, C, S, Alg]
    ): Out[HNil, (A, (B, C)), A]

    def gradp[
      A, B, C
    ](
      g: Split.Diff[A, B, C, S, Alg]
    ): Out[HNil, (A, (B, C)), HNil]
  }

  trait Dsl[S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out]] {

    def split[
      B, C
    ](
      implicit merger0: Merger[B, C]
    ): Dag[HNil, merger0.Out, (B, C), S, Alg] = new Split[merger0.Out, B, C, S, Alg] {
      val merger = merger0
    }
  
  }


  trait DiffDsl[
    S, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ] {

    def split[
      A, B, C
    ](implicit
      merger0: Merger.Aux[B, C, A]
    , costA: CostDiffInvertBuilder[A, S, Alg]
    , costDiffBC: CostDiffBuilder[(B, C), S, Alg]
    // , normAA: NormBuilder[(A, A), S, Alg]
    , scalarTimesHNil: ScalarTimesBuilder[HNil, S, Alg]
    , minusHNil: MinusPBuilder[HNil, S, Alg]
    , minusA0: MinusBuilder[A, S, Alg]
    ): DiffDag[HNil, A, (B, C), S, Alg] = new Split.Diff[A, B, C, S, Alg] {
      val merger = merger0
      val costDiff = costDiffBC
      val costDiffInvert = costA
      val scalarTimes = scalarTimesHNil
      val minusP = minusHNil
      val minusA = minusA0
    }

  }
}

