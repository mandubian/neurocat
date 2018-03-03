package neurocat
package idag

import shapeless.HNil


class Const[A, S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out]](val value: () => A)
extends Dag[HNil, Unit, A, S, Alg] {
  self =>

  def compile[Out[p, a, b]](
    compiler: Alg[Out]
  ): Out[HNil, Unit, A] = compiler.compile(self)

}


object Const {

  abstract class Diff[
    A
  , S
  // the algebra visitor
  , Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ](override val value: () => A)
  extends Const[A, S, Alg](value) with DiffDag[HNil, Unit, A, S, Alg] {
    self =>

    def gradA: Dag[HNil, (Unit, A), Unit, S, Alg] = new Dag[HNil, (Unit, A), Unit, S, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[HNil, (Unit, A), Unit] = compiler.grada(self)
    }

    def gradP: Dag[HNil, (Unit, A), HNil, S, Alg] = new Dag[HNil, (Unit, A), HNil, S, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[HNil, (Unit, A), HNil] = compiler.gradp(self)
    }

  }


  trait Algebra[
    S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out], Out[p, a, b]
  ] {
    def compile[A](
      dag: Const[A, S, Alg]
    ): Out[HNil, Unit, A]
  }

  trait DiffAlgebra[S, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out], Out[p, a, b]]
  extends Algebra[S, Alg, Out] {

    def grada[A](
      g: Const.Diff[A, S, Alg]
    ): Out[HNil, (Unit, A), Unit]

    def gradp[A](
      g: Const.Diff[A, S, Alg]
    ): Out[HNil, (Unit, A), HNil]
  }


  trait Dsl[S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out]] {

    def const[A](a: => A): Dag[HNil, Unit, A, S, Alg] = new Const(() => a)
  }


  trait DiffDsl[
    S, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ] {

    def const[A](a: => A)(
      implicit
        costDiffA: CostDiffBuilder[A, S, Alg]
      , costDiffInvertUnit: CostDiffInvertBuilder[Unit, S, Alg]
      , scalarTimesHNil: ScalarTimesBuilder[HNil, S, Alg]
      , minusHNil: MinusPBuilder[HNil, S, Alg]
      , minusUnit: MinusBuilder[Unit, S, Alg]
      // , normA: NormBuilder[A, S, Alg]
    ): DiffDag[HNil, Unit, A, S, Alg] = new Const.Diff[A, S, Alg](() => a) {
      val costDiffInvert = costDiffInvertUnit
      val costDiff = costDiffA
      val scalarTimes = scalarTimesHNil
      val minusP = minusHNil
      val minusA = minusUnit
      // val norm = normA
    }
  }
}

