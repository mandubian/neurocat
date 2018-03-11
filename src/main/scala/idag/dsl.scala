package neurocat
package idag

import typeclasses._
import algebra.ring.{AdditiveSemigroup, AdditiveGroup}
import shapeless.HNil


trait DagDsl[
  S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out]
]
extends Id.Dsl[S, Alg]
with Compose.Dsl[S, Alg]
with Prod.Dsl[S, Alg]
with Apply.Dsl[S, Alg]
with Fst.Dsl[S, Alg]
with Snd.Dsl[S, Alg]
with Const.Dsl[S, Alg]
with Split.Dsl[S, Alg]
with Join.Dsl[S, Alg]
with SlideR.Dsl[S, Alg]
with SlideL.Dsl[S, Alg] {

  def par[
    P, PA, PB
  , Q, QA, QB
  ](f: Dag[P, PA, PB, S, Alg], g: Dag[Q, QA, QB, S, Alg])(
    implicit merger0: Merger[P, Q]
  ): Dag[merger0.Out, (PA, QA), (PB, QB), S, Alg] = {
    prod(
      compose(f, fst[PA, QA])
    , compose(g, snd[PA, QA])
    )
  }

  def assocR[
    A, B, C
  ]: Dag[HNil, ((A, B), C), (A, (B, C)), S, Alg] = {
    val pa = compose(fst[A, B], fst[(A, B), C])
    val pb = compose(snd[A, B], fst[(A, B), C])
    val pc = snd[(A, B), C]
    prod(pa, prod(pb, pc))
  }

  def assocL[
    A, B, C
  ]: Dag[HNil, (A, (B, C)), ((A, B), C), S, Alg] = {
    val pa = fst[A, (B, C)]
    val pb = compose(fst[B, C], snd[A, (B, C)])
    val pc = compose(snd[B, C], snd[A, (B, C)])
    prod(prod(pa, pb), pc)
  }


  def flip[
    A , B
  ]: Dag[HNil, (A, B), (B, A), S, Alg] =
    prod(snd[A, B], fst[A, B])


  def interleave[A, B, C, D]: Dag[HNil, ((A, B), (C, D)), ((A, C), (B, D)), S, Alg] = {
    val in = par(par(id[A], id[B]), par(id[C], id[D]))
    val in2 = compose(assocL[A, C, (B, D)], par(id[A], compose(assocR[C, B, D], compose(par(flip[B, C], id[D]), compose(assocL[B, C, D], par(id[B], par(id[C], id[D])))))))
    compose(in2, compose(assocR[A, B, (C, D)], in))    
  }
}



trait DiffDagDsl[
  S, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
]
extends Id.DiffDsl[S, Alg]
with Compose.DiffDsl[S, Alg]
with Prod.DiffDsl[S, Alg]
with Fst.DiffDsl[S, Alg]
with Snd.DiffDsl[S, Alg]
with Const.DiffDsl[S, Alg]
with Split.DiffDsl[S, Alg]
with Join.DiffDsl[S, Alg] 
with SlideR.DiffDsl[S, Alg]
with SlideL.DiffDsl[S, Alg] {

  def par[
    P, PA : AdditiveGroup, PB
  , Q, QA : AdditiveGroup, QB
  ](f: DiffDag[P, PA, PB, S, Alg], g: DiffDag[Q, QA, QB, S, Alg])(
    implicit
    merger0: Merger[P, Q]
  , costPAQA: CostDiffInvertBuilder[(PA, QA), S, Alg]
  , costPA: CostDiffBuilder[PA, S, Alg]
  , costPB: CostDiffBuilder[PB, S, Alg]
  , costQA: CostDiffBuilder[QA, S, Alg]
  , costQB: CostDiffBuilder[QB, S, Alg]
  , costPBQB: CostDiffBuilder[(PB, QB), S, Alg]
  , scalarTimesHNil: ScalarTimesBuilder[HNil, S, Alg]
  , minusHNil: MinusPBuilder[HNil, S, Alg]
  , minusPAQA: MinusBuilder[(PA, QA), S, Alg]
  , minusP: MinusPBuilder[P, S, Alg]
  , minusQ: MinusPBuilder[Q, S, Alg]
  , scalarTimesP: ScalarTimesBuilder[P, S, Alg]
  , scalarTimesQ: ScalarTimesBuilder[Q, S, Alg]
  ): DiffDag[merger0.Out, (PA, QA), (PB, QB), S, Alg] = {
    val a = compose(f, fst[PA, QA])
    val b = compose(g, snd[PA, QA])
    prod(a, b)
  }

  def assocR[
    A : AdditiveGroup, B : AdditiveGroup, C : AdditiveGroup
  ](implicit
    costA: CostDiffBuilder[A, S, Alg]
  , costIA: CostDiffInvertBuilder[A, S, Alg]
  , costB: CostDiffBuilder[B, S, Alg]
  , costIB: CostDiffInvertBuilder[B, S, Alg]
  , costC: CostDiffBuilder[C, S, Alg]
  , costIC: CostDiffInvertBuilder[C, S, Alg]
  , scalarTimesHNil: ScalarTimesBuilder[HNil, S, Alg]
  , minusHNil: MinusPBuilder[HNil, S, Alg]
  , minusA: MinusBuilder[A, S, Alg]
  , minusB: MinusBuilder[B, S, Alg]
  , minusC: MinusBuilder[C, S, Alg]
  ): DiffDag[HNil, ((A, B), C), (A, (B, C)), S, Alg] = {
    val pa = compose(fst[A, B], fst[(A, B), C])
    val pb = compose(snd[A, B], fst[(A, B), C])
    val pc = snd[(A, B), C]
    prod(pa, prod(pb, pc))
  }

  def assocL[
    A : AdditiveGroup, B : AdditiveGroup, C : AdditiveGroup
  ](
    implicit
    costA: CostDiffBuilder[A, S, Alg]
  , costIA: CostDiffInvertBuilder[A, S, Alg]
  , costB: CostDiffBuilder[B, S, Alg]
  , costIB: CostDiffInvertBuilder[B, S, Alg]
  , costC: CostDiffBuilder[C, S, Alg]
  , costIC: CostDiffInvertBuilder[C, S, Alg]
  , scalarTimesHNil: ScalarTimesBuilder[HNil, S, Alg]
  , minusHNil: MinusPBuilder[HNil, S, Alg]
  , minusA: MinusBuilder[A, S, Alg]
  , minusB: MinusBuilder[B, S, Alg]
  , minusC: MinusBuilder[C, S, Alg]
  ): DiffDag[HNil, (A, (B, C)), ((A, B), C), S, Alg] = {
    val pa = fst[A, (B, C)]
    val pb = compose(fst[B, C], snd[A, (B, C)])
    val pc = compose(snd[B, C], snd[A, (B, C)])
    prod(prod(pa, pb), pc)
  }


  def flip[
    A : AdditiveGroup , B : AdditiveGroup
  ](
    implicit
    costA: CostDiffBuilder[A, S, Alg]
  , costIA: CostDiffInvertBuilder[A, S, Alg]
  , costB: CostDiffBuilder[B, S, Alg]
  , costIB: CostDiffInvertBuilder[B, S, Alg]
  , scalarTimesHNil: ScalarTimesBuilder[HNil, S, Alg]
  , minusHNil: MinusPBuilder[HNil, S, Alg]
  , minusA: MinusBuilder[A, S, Alg]
  , minusB: MinusBuilder[B, S, Alg]
  ): DiffDag[HNil, (A, B), (B, A), S, Alg] =
    prod(snd[A, B], fst[A, B])
}