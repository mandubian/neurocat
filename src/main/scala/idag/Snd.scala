package neurocat
package idag

import shapeless.HNil
import typeclasses._


class Snd[
  A, B
, S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out]
]() extends Dag[HNil, (A, B), B, S, Alg] {
  self =>

  def compile[Out[p, a, b]](compiler: Alg[Out]): Out[HNil, (A, B), B] = compiler.compile(this)

}

object Snd {


  abstract class Diff[
    A : Zeroed
  , B
  , S
  // algebra visitor
  , Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ]()
  extends Snd[A, B, S, Alg] with DiffDag[HNil, (A, B), B, S, Alg] {
    self =>

    def gradA: Dag[HNil, ((A, B), B), (A, B), S, Alg] =
      new Dag[HNil, ((A, B), B), (A, B), S, Alg] {
        def compile[Out[p, a, b]](
          compiler: Alg[Out]
        ): Out[HNil, ((A, B), B), (A, B)] = compiler.grada(self)
      }

    def gradP: Dag[HNil, ((A, B), B), HNil, S, Alg] =
      new Dag[HNil, ((A, B), B), HNil, S, Alg] {
        def compile[Out[p, a, b]](
          compiler: Alg[Out]
        ): Out[HNil, ((A, B), B), HNil] = compiler.gradp(self)
      }

  }

  trait Algebra[S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out], Out[p, a, b]] {      
    def compile[A, B](
      dag: Snd[A, B, S, Alg]
    ): Out[HNil, (A, B), B]
  }


  trait DiffAlgebra[
    S, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out], Out[p, a, b]
  ]
  extends Algebra[S, Alg, Out] {

    def grada[
      A : Zeroed, B
    ](
      g: Snd.Diff[A, B, S, Alg]
    ): Out[HNil, ((A, B), B), (A, B)]

    def gradp[
      A, B
    ](
      g: Snd.Diff[A, B, S, Alg]
    ): Out[HNil, ((A, B), B), HNil]
  }

  trait Dsl[S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out]] {

    def snd[A, B](): Dag[HNil, (A, B), B, S, Alg] = new Snd()

  }



  trait DiffDsl[
    S, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ] {

    def snd[
      A : Zeroed, B
    ](implicit
      costDiffB: CostDiffBuilder[B, S, Alg]
    , costDiffInvertAB: CostDiffInvertBuilder[(A, B), S, Alg]
    // , normB: NormBuilder[B, S, Alg]
    , scalarTimesHNil: ScalarTimesBuilder[HNil, S, Alg]
    , minusHNil: MinusPBuilder[HNil, S, Alg]
    , minusAB: MinusBuilder[(A, B), S, Alg]
    ): DiffDag[HNil, (A, B), B, S, Alg] = new Snd.Diff[A, B, S, Alg]{
      val costDiffInvert = costDiffInvertAB
      val costDiff = costDiffB
      // val norm = normB
      val scalarTimes = scalarTimesHNil
      val minusP = minusHNil
      val minusA = minusAB
    }
    
  }  
}