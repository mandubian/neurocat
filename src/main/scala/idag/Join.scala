package neurocat
package idag

import shapeless.HNil
import typeclasses._


abstract class Join[A, B, C, S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out]]
extends Dag[HNil, (A, B), C, S, Alg] {
  self =>

  def merger: Merger.Aux[A, B, C]

  def compile[Out[p, a, b]](
    compiler: Alg[Out]
  ): Out[HNil, (A, B), C] = compiler.compile(self)

}


object Join {

  abstract class Diff[
    A, B, C
  // the algebra visitor
  , S, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ]()
  extends Join[A, B, C, S, Alg] with DiffDag[HNil, (A, B), C, S, Alg] {
    self =>

    def gradA: Dag[HNil, ((A, B), C), (A, B), S, Alg] = new Dag[HNil, ((A, B), C), (A, B), S, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[HNil, ((A, B), C), (A, B)] = compiler.grada(self)
    }

    def gradP: Dag[HNil, ((A, B), C), HNil, S, Alg] = new Dag[HNil, ((A, B), C), HNil, S, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[HNil, ((A, B), C), HNil] = compiler.gradp(self)
    }

  }

  trait Algebra[S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out], Out[p, a, b]] {      
    def compile[A, B, C](
      dag: Join[A, B, C, S, Alg]
    ): Out[HNil, (A, B), C]
  }


  trait DiffAlgebra[
    S, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out], Out[p, a, b]
  ] extends Algebra[S, Alg, Out] {

    def grada[
      A, B, C
    ](
      dag: Join.Diff[A, B, C, S, Alg]
    ): Out[HNil, ((A, B), C), (A, B)]

    def gradp[
      A, B, C
    ](
      dag: Join.Diff[A, B, C, S, Alg]
    ): Out[HNil, ((A, B), C), HNil]
  }

  trait Dsl[S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out]] {

    def join[A, B](
      implicit merger0: Merger[A, B]
    ): Dag[HNil, (A, B), merger0.Out, S, Alg] = new Join[A, B, merger0.Out, S, Alg] {
      val merger = merger0
    }
  }



  trait DiffDsl[
    S, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ] {

    def join[A, B, C](
      implicit
        merger0: Merger.Aux[A, B, C]
      , costDiffC: CostDiffBuilder[C, S, Alg]
      , costDiffInvertAB: CostDiffInvertBuilder[(A, B), S, Alg]
      , scalarTimesHNil: ScalarTimesBuilder[HNil, S, Alg]
      , minusHNil: MinusPBuilder[HNil, S, Alg]
      , minusAB: MinusBuilder[(A, B), S, Alg]
      // , normC: NormBuilder[C, S, Alg]
    ): DiffDag[HNil, (A, B), C, S, Alg] = new Join.Diff[A, B, C, S, Alg] {
      val merger = merger0
      val costDiffInvert = costDiffInvertAB
      val costDiff = costDiffC
      // val norm = normC
      val scalarTimes = scalarTimesHNil
      val minusP = minusHNil
      val minusA = minusAB
    }
  }
}

