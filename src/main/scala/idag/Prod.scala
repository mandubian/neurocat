package neurocat
package idag

import shapeless.HNil
import typeclasses._
import algebra.ring.AdditiveSemigroup

/*   
 *                       PB
 *  P|PA -> PB          /
 *     **      =>  PQ|PA
 *  Q|PA -> QB          \
 *                       QC
 */
abstract class Prod[
  P, PA, PB
, Q, QB
// merging P & Q params
, PQ
// the algebra visitor
, S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out]
] private[neurocat] (
  val f: Dag[P, PA, PB, S, Alg]
, val g: Dag[Q, PA, QB, S, Alg]
) extends Dag[PQ, PA, (PB, QB), S, Alg] {
  self =>
  // the merger of P & Q
  def merger: Merger.Aux[P, Q, PQ]

  def compile[Out[p, a, b]](
    compiler: Alg[Out]
  ): Out[PQ, PA, (PB, QB)] = compiler.compile(self)

}


object Prod {

  abstract class Diff[
  // PA requires an AdditiveSemigroup for grada 2 branches back-propagation
    P, PA : AdditiveSemigroup, PB
  , Q, QB
  , PQ
  , S
  // algebra visitor
  , Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ] private[neurocat] (
    override val f: DiffDag[P, PA, PB, S, Alg]
  , override val g: DiffDag[Q, PA, QB, S, Alg]
  ) extends Prod[P, PA, PB, Q, QB, PQ, S, Alg](f, g) with DiffDag[PQ, PA, (PB, QB), S, Alg] {
    self =>

    def gradA: Dag[PQ, (PA, (PB, QB)), PA, S, Alg] = new Dag[PQ, (PA, (PB, QB)), PA, S, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[PQ, (PA, (PB, QB)), PA] = compiler.grada(self)
    }

    def gradP: Dag[PQ, (PA, (PB, QB)), PQ, S, Alg] = new Dag[PQ, (PA, (PB, QB)), PQ, S, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[PQ, (PA, (PB, QB)), PQ] = compiler.gradp(self)
    }    
  }

  trait Algebra[S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out], Out[p, a, b]] {
    def compile[
        P, PA, PB
      , Q, QB
      , PQ
      ](
        dag: Prod[P, PA, PB, Q, QB, PQ, S, Alg]
      ): Out[PQ, PA, (PB, QB)]
  }


  trait DiffAlgebra[
    S, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out], Out[p, a, b]
  ]
  extends Algebra[S, Alg, Out] {

    def grada[
      P, PA : AdditiveSemigroup, PB
    , Q, QB
    , PQ
    ](
      dag: Prod.Diff[P, PA, PB, Q, QB, PQ, S, Alg]
    ): Out[PQ, (PA, (PB, QB)), PA]

    def gradp[
      P, PA : AdditiveSemigroup, PB
    , Q, QB
    , PQ
    ](
      dag: Prod.Diff[P, PA, PB, Q, QB, PQ, S, Alg]
    ): Out[PQ, (PA, (PB, QB)), PQ]
  }

  trait Dsl[S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out]] {

    def prod[
        P, PA, PB, Q, QB, QGP
      , PQ
    ](
      f: Dag[P, PA, PB, S, Alg]
    , g: Dag[Q, PA, QB, S, Alg]
    )(
      implicit merger0: Merger[P, Q]
    ): Dag[merger0.Out, PA, (PB, QB), S, Alg]
    = new Prod[
        P, PA, PB, Q, QB
      , merger0.Out
      , S, Alg
      ](f, g) {
        def merger = merger0
      }

  }


  trait DiffDsl[
    S, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ] {

    def prod[
        P, PA : AdditiveSemigroup, PB, Q, QB, QGP
      , PQ
    ](
      f: DiffDag[P, PA, PB, S, Alg]
    , g: DiffDag[Q, PA, QB, S, Alg]
    )(
      implicit
        merger0: Merger.Aux[P, Q, PQ]
      , costPA: CostDiffInvertBuilder[PA, S, Alg]
      , costPBQB: CostDiffBuilder[(PB, QB), S, Alg]
      , scalarTimesPQ: ScalarTimesBuilder[PQ, S, Alg]
      , minusPQ: MinusPBuilder[PQ, S, Alg]
      , minusPA: MinusBuilder[PA, S, Alg]
    ): DiffDag[PQ, PA, (PB, QB), S, Alg]
    = new Prod.Diff[
        P, PA, PB, Q, QB
      , PQ
      , S, Alg
      ](f, g) {
        val merger = merger0
        val costDiffInvert = costPA
        val costDiff = costPBQB
        val scalarTimes = scalarTimesPQ
        val minusP = minusPQ
        val minusA = minusPA
      }

  }
}