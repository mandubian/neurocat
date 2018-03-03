package neurocat
package idag
package nd4j


import shapeless.{::, HNil}
import typeclasses._
import neurocat.nd4j._
  
object L2 {
  implicit def costBuilder[S, D <: Dim, Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out]] =
    new CostBuilder[Mat[S, D], S, Alg] {
      def apply() = new CostL2()
    }

  implicit def costDiffBuilder[S, D <: Dim, Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out]] =
    new CostDiffBuilder[Mat[S, D], S, Alg] {
      def apply() = new CostL2Diff()
    }

  implicit def costDiffInvertBuilder[S, D <: Dim, Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out]] =
    new CostDiffInvertBuilder[Mat[S, D], S, Alg] {
      def apply() = new CostL2DiffInvert()
    }

  implicit def scalarTimesBuilder[S, D <: Dim, Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out]] =
    new ScalarTimesBuilder[Mat[S, D], S, Alg] {
      def apply() = new ScalarTimes()
    }
}




class CostL2[
  S, D <: Dim
, Alg[out[p, a, b]] <: nd4j.Algebra[S, Alg, out]
] extends Dag[HNil, (Mat[S, D], Mat[S, D]), Mat[S, D], S, Alg] {
  self =>
  def compile[Out[p, a, b]](
    compiler: Alg[Out]
  ): Out[HNil, (Mat[S, D], Mat[S, D]), Mat[S, D]] =
    compiler.compile(self)
}

object CostL2 {

  trait Algebra[S, Alg[out[p, a, b]] <: nd4j.Algebra[S, Alg, out], Out[p, a, b]]
  extends DagAlgebra[S, Alg, Out] {

    def compile[D <: Dim](dag: CostL2[S, D, Alg]): Out[HNil, (Mat[S, D], Mat[S, D]), Mat[S, D]]

  }

}

class CostL2Diff[
  S, D <: Dim
, Alg[out[p, a, b]] <: nd4j.Algebra[S, Alg, out]
] extends Dag[HNil, (Mat[S, D], Mat[S, D]), Mat[S, D], S, Alg] {
  self =>
  def compile[Out[p, a, b]](
    compiler: Alg[Out]
  ): Out[HNil, (Mat[S, D], Mat[S, D]), Mat[S, D]] =
    compiler.compile(self)
}

object CostL2Diff {

  trait Algebra[S, Alg[out[p, a, b]] <: nd4j.Algebra[S, Alg, out], Out[p, a, b]]
  extends DagAlgebra[S, Alg, Out] {

    def compile[D <: Dim](dag: CostL2Diff[S, D, Alg]): Out[HNil, (Mat[S, D], Mat[S, D]), Mat[S, D]]

  }

}

class CostL2DiffInvert[
  S, D <: Dim
, Alg[out[p, a, b]] <: nd4j.Algebra[S, Alg, out]
] extends Dag[HNil, (Mat[S, D], Mat[S, D]), Mat[S, D], S, Alg] {
  self =>
  def compile[Out[p, a, b]](
    compiler: Alg[Out]
  ): Out[HNil, (Mat[S, D], Mat[S, D]), Mat[S, D]] =
    compiler.compile(self)
}

object CostL2DiffInvert {

  trait Algebra[S, Alg[out[p, a, b]] <: nd4j.Algebra[S, Alg, out], Out[p, a, b]]
  extends DagAlgebra[S, Alg, Out] {

    def compile[D <: Dim](dag: CostL2DiffInvert[S, D, Alg]): Out[HNil, (Mat[S, D], Mat[S, D]), Mat[S, D]]

  }

}

