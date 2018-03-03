package neurocat
package idag

import shapeless.HNil
import typeclasses._


class Fst[
  A, B
// the algebra visitor
, S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out]
]() extends Dag[HNil, (A, B), A, S, Alg] {
  self =>

  def compile[Out[p, a, b]](compiler: Alg[Out]): Out[HNil, (A, B), A] = compiler.compile(this)

}

object Fst {

  abstract class Diff[
    A, B : Zeroed
  , S
  // the algebra visitor
  , Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ] extends Fst[A, B, S, Alg] with DiffDag[HNil, (A, B), A, S, Alg] {
    self =>

    def gradA: Dag[HNil, ((A, B), A), (A, B), S, Alg] =
      new Dag[HNil, ((A, B), A), (A, B), S, Alg] {
        def compile[Out[p, a, b]](
          compiler: Alg[Out]
        ): Out[HNil, ((A, B), A), (A, B)] = compiler.grada(self)
      }

    def gradP: Dag[HNil, ((A, B), A), HNil, S, Alg] =
      new Dag[HNil, ((A, B), A), HNil, S, Alg] {
        def compile[Out[p, a, b]](
          compiler: Alg[Out]
        ): Out[HNil, ((A, B), A), HNil] = compiler.gradp(self)
      }

  }


  trait Algebra[S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out], Out[p, a, b]] {      
    def compile[A, B](
      dag: Fst[A, B, S, Alg]
    ): Out[HNil, (A, B), A]
  }


  trait DiffAlgebra[
    S, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out], Out[p, a, b]
  ]
  extends Algebra[S, Alg, Out] {

    def grada[
      A, B : Zeroed
    ](
      dag: Fst.Diff[A, B, S, Alg]
    ): Out[HNil, ((A, B), A), (A, B)]

    def gradp[
      A, B
    ](
      dag: Fst.Diff[A, B, S, Alg]
    ): Out[HNil, ((A, B), A), HNil]
  }

  trait Dsl[S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out]] {

    def fst[
      A, B
    ]: Dag[HNil, (A, B), A, S, Alg] = new Fst[A, B, S, Alg]() 

  }


  trait DiffDsl[
    S, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ] {

    def fst[
      A, B : Zeroed
    ](
      implicit
        costDiffA: CostDiffBuilder[A, S, Alg]
      , costDiffInvertAB: CostDiffInvertBuilder[(A, B), S, Alg]
      , scalarTimesHNil: ScalarTimesBuilder[HNil, S, Alg]
      , minusHNil: MinusPBuilder[HNil, S, Alg]
      , minusAB: MinusBuilder[(A, B), S, Alg]
      // , normA0: NormBuilder[A, S, Alg]
    ): DiffDag[HNil, (A, B), A, S, Alg] = new Fst.Diff[A, B, S, Alg]() {
      val costDiff = costDiffA
      val costDiffInvert = costDiffInvertAB
      // val norm = normA0
      val scalarTimes = scalarTimesHNil
      val minusA = minusAB
      val minusP = minusHNil
    }
  }  
}