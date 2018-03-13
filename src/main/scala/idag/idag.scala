package neurocat
package idag

import shapeless.HNil
import typeclasses._


trait Dag[
  P, A, B
// algebra visitor
, S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out]
] {

  def compile[Out[p, a, b]](
    compiler: Alg[Out]
  ): Out[P, A, B]

}


trait DiffDag[
  P, A, B
, S, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
] extends Dag[P, A, B, S, Alg] {
  self =>

  def costDiffInvert: CostDiffInvertBuilder[A, S, Alg]
  def costDiff: CostDiffBuilder[B, S, Alg]
  def scalarTimes: ScalarTimesBuilder[P, S, Alg]
  def minusP: MinusPBuilder[P, S, Alg]
  def minusA: MinusBuilder[A, S, Alg]

  def gradP: Dag[P, (A, B), P, S, Alg]

  def gradA: Dag[P, (A, B), A, S, Alg]

  def >>:[Q, C, QGP, QGA](other: DiffDag[Q, C, A, S, Alg])(
    implicit
      merger: Merger[Q, P]
  ): DiffDag[merger.Out, C, B, S, Alg] = Compose.compose(self, other)

}
