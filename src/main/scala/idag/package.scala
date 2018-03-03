package neurocat

import algebra.ring.{AdditiveSemigroup, AdditiveGroup}
import typeclasses._
import shapeless.HNil


package object idag {
  implicit class DiffDagOps[
    P, PA, PB, S, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ](
    dag: DiffDag[P, PA, PB, S, Alg]
  ) extends DiffDagDsl[S, Alg] {

    def >>>[Q, QC, QGP, QGA, PQ](other: DiffDag[Q, PB, QC, S, Alg])(
      implicit
        merger: Merger.Aux[P, Q, PQ]
      , cpa: CostBuilder[PA, S, Alg], cqc: CostBuilder[QC, S, Alg]
      , costDiffInvertA: CostDiffInvertBuilder[PA, S, Alg]
      , costDiffC: CostDiffBuilder[QC,S,Alg]
      , minusPQ: MinusPBuilder[PQ, S, Alg]
      , minusA0: MinusBuilder[PA, S, Alg]
      , scalarTimesPQ: ScalarTimesBuilder[PQ, S, Alg]
    ): DiffDag[PQ, PA, QC, S, Alg] = compose(other, dag)

    def **[Q, QB, QGP, QGA, PQ](other: DiffDag[Q, PA, QB, S, Alg])(
      implicit
        merger: Merger.Aux[P, Q, PQ]
      , paadd : AdditiveGroup[PA]
      , cpb: CostDiffBuilder[PB, S, Alg]
      // , scalarTimesPB: ScalarTimesBuilder[PB, S, Alg]
      , cqb: CostDiffBuilder[QB, S, Alg]
      , costDiffInvertPA: CostDiffInvertBuilder[PA, S, Alg]
      , minusPQ: MinusPBuilder[PQ, S, Alg]
      , minusPA: MinusBuilder[PA, S, Alg]
      , scalarTimesPQ: ScalarTimesBuilder[PQ, S, Alg]
    ): DiffDag[PQ, PA, (PB, QB), S, Alg] = prod(dag, other)

    def ||[Q, QA : AdditiveGroup, QB, PQ](other: DiffDag[Q, QA, QB, S, Alg])(
      implicit
        merger: Merger.Aux[P, Q, PQ]
      , paadd : AdditiveGroup[PA]
      , costDiffPA: CostDiffBuilder[PA, S, Alg]
      , costDiffPB: CostDiffBuilder[PB, S, Alg]
      , costDiffQA: CostDiffBuilder[QA, S, Alg]
      , costDiffQB: CostDiffBuilder[QB, S, Alg]
      , costDiffInvertPA: CostDiffInvertBuilder[PA, S, Alg]
      , costDiffInvertQA: CostDiffInvertBuilder[QA, S, Alg]
      , scalarTimesHNil: ScalarTimesBuilder[HNil, S, Alg]
      , scalarTimesP: ScalarTimesBuilder[P, S, Alg]
      , scalarTimesQ: ScalarTimesBuilder[Q, S, Alg]
      , minusHNil: MinusPBuilder[HNil, S, Alg]
      , minusP: MinusPBuilder[P, S, Alg]
      , minusQ: MinusPBuilder[Q, S, Alg]
      , minusPA: MinusBuilder[PA, S, Alg]
      , minusQA: MinusBuilder[QA, S, Alg]
    ): DiffDag[PQ, (PA, QA), (PB, QB), S, Alg] = par(dag, other)
  }



  implicit class DagOps[P, PA, PB, S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out]](
    dag: Dag[P, PA, PB, S, Alg]
  ) extends DagDsl[S, Alg] {

    def >>>[Q, QC, QGP, QGA](other: Dag[Q, PB, QC, S, Alg])(
      implicit merger: Merger[P, Q]
    ): Dag[merger.Out, PA, QC, S, Alg] = compose(other, dag)

    def **[Q, QB, QGP, QGA](other: Dag[Q, PA, QB, S, Alg])(
      implicit merger: Merger[P, Q]
    ): Dag[merger.Out, PA, (PB, QB), S, Alg] = prod(dag, other)

    def ||[Q, QA, QB](other: Dag[Q, QA, QB, S, Alg])(
      implicit
      merger: Merger[P, Q]
    ): Dag[merger.Out, (PA, QA), (PB, QB), S, Alg] = par(dag, other)
  }



  implicit def tupleAdditiveSemigroup[A : AdditiveSemigroup, B : AdditiveSemigroup] =
    new AdditiveSemigroup[(A, B)] {
      def plus(x: (A, B), y: (A, B)): (A, B) = (AdditiveSemigroup[A].plus(x._1, y._1), AdditiveSemigroup[B].plus(x._2, y._2))
    }

  implicit def tupleZero[A : Zeroed, B : Zeroed] =
    new Zeroed[(A, B)] {
      def zero: (A, B) = (Zeroed[A].zero, Zeroed[B].zero)
    }

  implicit def additiveGroup2Zero[A : AdditiveGroup] = new Zeroed[A] {
    def zero = AdditiveGroup[A].zero
  }
}

