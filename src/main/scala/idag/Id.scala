package neurocat
package idag

import shapeless.HNil


trait Id[A
// algebra visitor
, S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out]
] extends Dag[HNil, A, A, S, Alg] {
  self =>

  def compile[Out[p, a, b]](
    compiler: Alg[Out]
  ): Out[HNil, A, A] = compiler.compile(self)

}


object Id {


  trait Diff[
    A
  , S
  // algebra visitor
  , Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ] extends Id[A, S, Alg] with DiffDag[HNil, A, A, S, Alg] {
    self =>

    def gradA: Dag[HNil, (A, A), A, S, Alg] = new Dag[HNil, (A, A), A, S, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[HNil, (A, A), A] = compiler.grada(self)
    }

    def gradP: Dag[HNil, (A, A), HNil, S, Alg] = new Dag[HNil, (A, A), HNil, S, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[HNil, (A, A), HNil] = compiler.gradp(self)
    }
  }


  trait Algebra[S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out], Out[p, a, b]] {
    def compile[A](
      dag: Id[A, S, Alg]
    ): Out[HNil, A, A]
  }



  trait DiffAlgebra[
    S, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out], Out[p, a, b]
  ] extends Algebra[S, Alg, Out] {

    def grada[A](
      g: Id.Diff[A, S, Alg]
    ): Out[HNil, (A, A), A]

    def gradp[A](
      g: Id.Diff[A, S, Alg]
    ): Out[HNil, (A, A), HNil]
  }

  trait Dsl[S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out]] {

    def id[A]: Dag[HNil, A, A, S, Alg] = new Id[A, S, Alg] {}
  }


  trait DiffDsl[
    S, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ] {

    def id[A](
      implicit
        costDiffA: CostDiffBuilder[A, S, Alg]
      , costDiffInvertA: CostDiffInvertBuilder[A, S, Alg]
      , scalarTimesHNil: ScalarTimesBuilder[HNil, S, Alg]
      , minusHNil: MinusPBuilder[HNil, S, Alg]
      , minusA0: MinusBuilder[A, S, Alg]
      // , normA: NormBuilder[A, S, Alg]
    ): DiffDag[HNil, A, A, S, Alg] =
      new Id.Diff[A, S, Alg] {
        val costDiff = costDiffA
        val costDiffInvert = costDiffInvertA
        val scalarTimes = scalarTimesHNil
        val minusA = minusA0
        val minusP = minusHNil
        // val norm = normA
      }
  }
}

