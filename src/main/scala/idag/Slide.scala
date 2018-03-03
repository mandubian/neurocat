package neurocat
package idag

import shapeless.HNil


class SlideR[
  P, A, B
// algebra visitor
, S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out]
](val dag: Dag[P, A, B, S, Alg]) extends Dag[HNil, (P, A), B, S, Alg] {
  self =>

  def compile[Out[p, a, b]](
    compiler: Alg[Out]
  ): Out[HNil, (P, A), B] = compiler.compile(self)

}

object SlideR {

  abstract class Diff[
    P, A, B
  , S
  // algebra visitor
  , Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ](override val dag: DiffDag[P, A, B, S, Alg]) extends SlideR[P, A, B, S, Alg](dag) with DiffDag[HNil, (P, A), B, S, Alg] {
    self =>

    def gradA: Dag[HNil, ((P, A), B), (P, A), S, Alg] = new Dag[HNil, ((P, A), B), (P, A), S, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[HNil, ((P, A), B), (P, A)] = compiler.grada(self)
    }

    def gradP: Dag[HNil, ((P, A), B), HNil, S, Alg] = new Dag[HNil, ((P, A), B), HNil, S, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[HNil, ((P, A), B), HNil] = compiler.gradp(self)
    }
  }


  trait Algebra[S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out], Out[p, a, b]] {
    def compile[P, A, B](
      dag: SlideR[P, A, B, S, Alg]
    ): Out[HNil, (P, A), B]
  }



  trait DiffAlgebra[
    S, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out], Out[p, a, b]
  ] extends Algebra[S, Alg, Out] {

    def grada[P, A, B](
      g: SlideR.Diff[P, A, B, S, Alg]
    ): Out[HNil, ((P, A), B), (P, A)]

    def gradp[P, A, B](
      g: SlideR.Diff[P, A, B, S, Alg]
    ): Out[HNil, ((P, A), B), HNil]
  }

  trait Dsl[S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out]] {

    def slideR[P, A, B](dag: Dag[P, A, B, S, Alg]): Dag[HNil, (P, A), B, S, Alg] = new SlideR[P, A, B, S, Alg](dag)
  }


  trait DiffDsl[
    S, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ] {

    def slideR[P, A, B](
      dag: DiffDag[P, A, B, S, Alg]
    )(
      implicit
        costDiffB: CostDiffBuilder[B, S, Alg]
      , costDiffInvertPA: CostDiffInvertBuilder[(P, A), S, Alg]
      , scalarTimesHNil: ScalarTimesBuilder[HNil, S, Alg]
      , minusHNil: MinusPBuilder[HNil, S, Alg]
      , minusPA: MinusBuilder[(P, A), S, Alg]
    ): DiffDag[HNil, (P, A), B, S, Alg] =
      new SlideR.Diff[P, A, B, S, Alg](dag) {
        val costDiff = costDiffB
        val costDiffInvert = costDiffInvertPA
        val scalarTimes = scalarTimesHNil
        val minusA = minusPA
        val minusP = minusHNil
        // val norm = normA
      }
  }
}



class SlideL[
  P, A, B
// algebra visitor
, S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out]
](val dag: Dag[HNil, (P, A), B, S, Alg]) extends Dag[P, A, B, S, Alg] {
  self =>

  def compile[Out[p, a, b]](
    compiler: Alg[Out]
  ): Out[P, A, B] = compiler.compile(self)

}

object SlideL {

  abstract class Diff[
    P, A, B
  , S
  // algebra visitor
  , Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ](override val dag: DiffDag[HNil, (P, A), B, S, Alg]) extends SlideL[P, A, B, S, Alg](dag) with DiffDag[P, A, B, S, Alg] {
    self =>

    def gradA: Dag[P, (A, B), A, S, Alg] = new Dag[P, (A, B), A, S, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[P, (A, B), A] = compiler.grada(self)
    }

    def gradP: Dag[P, (A, B), P, S, Alg] = new Dag[P, (A, B), P, S, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[P, (A, B), P] = compiler.gradp(self)
    }
  }


  trait Algebra[S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out], Out[p, a, b]] {
    def compile[P, A, B](
      dag: SlideL[P, A, B, S, Alg]
    ): Out[P, A, B]
  }



  trait DiffAlgebra[
    S, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out], Out[p, a, b]
  ] extends Algebra[S, Alg, Out] {

    def grada[P, A, B](
      g: SlideL.Diff[P, A, B, S, Alg]
    ): Out[P, (A, B), A]

    def gradp[P, A, B](
      g: SlideL.Diff[P, A, B, S, Alg]
    ): Out[P, (A, B), P]
  }

  trait Dsl[S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out]] {

    def slideL[P, A, B](dag: Dag[HNil, (P, A), B, S, Alg]): Dag[P, A, B, S, Alg] = new SlideL[P, A, B, S, Alg](dag)
  }


  trait DiffDsl[
    S, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ] {

    def slideL[P, A, B](
      dag: DiffDag[HNil, (P, A), B, S, Alg]
    )(
      implicit
        costDiffB: CostDiffBuilder[B, S, Alg]
      , costDiffInvertA: CostDiffInvertBuilder[A, S, Alg]
      , scalarTimesP: ScalarTimesBuilder[P, S, Alg]
      , minusP0: MinusPBuilder[P, S, Alg]
      , minusA0: MinusBuilder[A, S, Alg]
    ): DiffDag[P, A, B, S, Alg] =
      new SlideL.Diff[P, A, B, S, Alg](dag) {
        val costDiff = costDiffB
        val costDiffInvert = costDiffInvertA
        val scalarTimes = scalarTimesP
        val minusA = minusA0
        val minusP = minusP0
      }
  }
}