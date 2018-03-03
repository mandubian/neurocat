package neurocat
package idag
package nd4j


import shapeless.{::, HNil}
import typeclasses._
import neurocat.nd4j._
import singleton.ops._


class Sigmoid[
  S, R <: XInt, C <: XInt
, Alg[out[p, a, b]] <: nd4j.Algebra[S, Alg, out]
]()
extends Dag[HNil, Mat[S, R x C], Mat[S, R x C], S, Alg] {
  self =>
  def compile[Out[p, a, b]](
    compiler: Alg[Out]
  ): Out[HNil, Mat[S, R x C], Mat[S, R x C]] =
    compiler.compile(self)
}

object Sigmoid {

  abstract class Diff[
    S, R <: XInt, C <: XInt
  , Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out]
  ]()
  extends Sigmoid[S, R, C, Alg] with DiffDag[HNil, Mat[S, R x C], Mat[S, R x C], S, Alg] {
    self =>

   def gradA = new Dag[HNil, (Mat[S, R x C], Mat[S, R x C]), Mat[S, R x C], S, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[HNil, (Mat[S, R x C], Mat[S, R x C]), Mat[S, R x C]] = compiler.grada(self)
    }

    def gradP = new Dag[HNil, (Mat[S, R x C], Mat[S, R x C]), HNil, S, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[HNil, (Mat[S, R x C], Mat[S, R x C]), HNil] = compiler.gradp(self)
    }

  }

  trait Algebra[S, Alg[out[p, a, b]] <: nd4j.Algebra[S, Alg, out], Out[p, a, b]]
  extends DagAlgebra[S, Alg, Out] {

    def compile[
      R <: XInt, C <: XInt
    ](
      dag: Sigmoid[S, R, C, Alg]
    ): Out[HNil, Mat[S, R x C], Mat[S, R x C]]

  }

  trait DiffAlgebra[S, Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out], Out[p, a, b]]
  extends Algebra[S, Alg, Out] {

    def grada[
      R <: XInt, C <: XInt
    ](
      dag: Sigmoid.Diff[S, R, C, Alg]
    ): Out[HNil, (Mat[S, R x C], Mat[S, R x C]), Mat[S, R x C]]

    def gradp[
      R <: XInt, C <: XInt
    ](
      dag: Sigmoid.Diff[S, R, C, Alg]
    ): Out[HNil, (Mat[S, R x C], Mat[S, R x C]), HNil]
  }

  trait Dsl[S, Alg[out[p, a, b]] <: nd4j.Algebra[S, Alg, out]] {

    def sigmoid[R <: XInt, C <: XInt]: Dag[HNil, Mat[S, R x C], Mat[S, R x C], S, Alg] = new Sigmoid()

  }



  trait DiffDsl[
    S, Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out]
  ] {

    def sigmoid[R <: XInt, C <: XInt](
      implicit
        costDiffInvertRC: CostDiffInvertBuilder[Mat[S, R x C], S, Alg]
      , costDiffRC: CostDiffBuilder[Mat[S, R x C], S, Alg]
      , scalarTimesHNil: ScalarTimesBuilder[HNil, S, Alg]
      , minusA0: MinusBuilder[Mat[S, R x C], S, Alg]
      , minusP0: MinusPBuilder[HNil, S, Alg]
    ): DiffDag[HNil, Mat[S, R x C], Mat[S, R x C], S, Alg] =
      new Sigmoid.Diff[S, R, C, Alg] {
        val costDiffInvert = costDiffInvertRC
        val costDiff = costDiffRC
        val scalarTimes = scalarTimesHNil
        val minusA = minusA0
        val minusP = minusP0
      }
  }

}

