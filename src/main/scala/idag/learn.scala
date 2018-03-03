package neurocat
package idag

import shapeless.HNil
import typeclasses._
import algebra.ring.AdditiveSemigroup

  trait LearnAlgebra[
    S, Out[p, a, b]
  ] extends DiffDagAlgebra[S, Lambda[out[p, a, b] => LearnAlgebra[S, out]], Out]

  trait LearnCompiler0[
    S, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ] {
    self : Alg[Lambda[(p, a, b) => Dag[p, a, b, S, Alg]]] =>

    val eps: S

    val dsl0 = new DagDsl[S, Alg] {}
    import dsl0._

    type Out[p, a, b] = Dag[p, a, b, S, Alg]
    type LA[out[p, a, b]] = Alg[out]

    implicit def aCo[A] = new Comonoid[A] {
      def split(a: A): (A, A) = (a, a)
      def destroy(a: A) = ()
    }

    def compile[
      P, PA, PB
    , Q, QC
    , PQ
    ](
      dag: Compose[P, PA, PB, Q, QC, PQ, S, Alg]
    ): Dag[PQ, PA, QC, S, Alg] = dag

    def gradp[
      P, PA, PB
    , Q, QC
    , PQ
    ](
      dag: Compose.Diff[P, PA, PB, Q, QC, PQ, S, Alg]
    ): Dag[PQ, (PA, QC), PQ, S, Alg] = {
      implicit val m = dag.merger

      val idQC = id[QC]
      val idPA = id[PA]

      val pfi = dag.f.compile[Out](self)
      val pfu = dag.f.gradP.compile[Out](self)
      val pgu = dag.g.gradP.compile[Out](self)
      val pgr = dag.g.gradA.compile[Out](self)

      val pgr2 = (pfi || idQC) >>> pgr
      val u = ((split[PA, PA, PA] || idQC) >>> assocR[PA, PA, QC]) >>> ((idPA || pgr2) >>> pfu)

      val v = (pfi || idQC) >>> pgu
      (u ** v) >>> join
    }

    def grada[
      P, PA, PB
    , Q, QC
    , PQ
    ](
      dag: Compose.Diff[P, PA, PB, Q, QC, PQ, S, LA]
    ): Dag[PQ, (PA, QC), PA, S, LA] = {
      implicit val m = dag.merger

      val idQC = id[QC]
      val idPA = id[PA]

      val pfi = dag.f.compile[Out](self)
      val pfr = dag.f.gradA.compile[Out](self)
      val pgr = dag.g.gradA.compile[Out](self)
      val reqCompose = (pfi || idQC) >>> pgr

      val init = ((split[PA, PA, PA] || idQC) >>> assocR[PA, PA, QC])

      init >>> ((idPA || reqCompose) >>> pfr)
    }


//////////////////////////////////////////////////////////////////
// ID
    def compile[A](
      dag: Id[A, S, LA]
    ): Dag[HNil, A, A, S, LA] =
      compile0(dag)

    def gradp[A](
      dag: Id.Diff[A, S, LA]
    ): Dag[HNil, (A, A), HNil, S, LA] =
      gradp0(dag)

    def grada[A](
      dag: Id.Diff[A, S, LA]
    ): Dag[HNil, (A, A), A, S, LA] =
      grada0(dag)

//////////////////////////////////////////////////////////////////
// CONST
    def compile[A](
      dag: Const[A, S, LA]
    ): Dag[HNil, Unit, A, S, LA] =
      compile0(dag)

    def grada[A](
      dag: Const.Diff[A, S, LA]
    ): Dag[HNil, (Unit, A), Unit, S, LA] = 
      grada0(dag)

    def gradp[A](
      dag: Const.Diff[A, S, LA]
    ): Dag[HNil, (Unit, A), HNil, S, LA] =
      gradp0(dag)


//////////////////////////////////////////////////////////////////
// FST
    def compile[A, B](
      dag: Fst[A, B, S, LA]
    ): Dag[HNil, (A, B), A, S, LA] =
      compile0(dag)

    def grada[A, B : Zeroed](
      dag: Fst.Diff[A, B, S, LA]
    ): Dag[HNil, ((A, B), A), (A, B), S, LA] =
      grada0(dag)

    def gradp[A, B](
      dag: Fst.Diff[A, B, S, LA]
    ): Dag[HNil, ((A, B), A), HNil, S, LA] =
      gradp0(dag)

//////////////////////////////////////////////////////////////////
// SND
    def compile[A, B](
      dag: Snd[A, B, S, LA]
    ): Dag[HNil, (A, B), B, S, LA] =
      compile0(dag)

    def grada[A : Zeroed, B](
      dag: Snd.Diff[A, B, S, LA]
    ): Dag[HNil, ((A, B), B), (A, B), S, LA] =
      grada0(dag)

    def gradp[A, B](
      dag: Snd.Diff[A, B, S, LA]
    ): Dag[HNil, ((A, B), B), HNil, S, LA] =
      gradp0(dag)

//////////////////////////////////////////////////////////////////
// JOIN
    def compile[A, B, C](
      dag: Join[A, B, C, S, LA]
    ): Dag[HNil, (A, B), C, S, LA] =
      compile0(dag)

    def grada[A, B, C](
      dag: Join.Diff[A, B, C, S, LA]
    ): Dag[HNil, ((A, B), C), (A, B), S, LA] = 
      grada0(dag)

    def gradp[A, B, C](
      dag: Join.Diff[A, B, C, S, LA]
    ): Dag[HNil, ((A, B), C), HNil, S, LA] =
      gradp0(dag)

//////////////////////////////////////////////////////////////////
// SPLIT
    def compile[A, B, C](
      dag: Split[A, B, C, S, LA]
    ): Dag[HNil, A, (B, C), S, LA] =
      compile0(dag)

    def grada[A, B, C](
      dag: Split.Diff[A, B, C, S, LA]
    ): Dag[HNil, (A, (B, C)), A, S, LA] =
      grada0(dag)

    def gradp[A, B, C](
      dag: Split.Diff[A, B, C, S, LA]
    ): Dag[HNil, (A, (B, C)), HNil, S, LA] =
      gradp0(dag)

//////////////////////////////////////////////////////////////////
// PROD
    def compile[
      P, PA, PB
    , Q, QB
    , PQ
    ](
      dag: Prod[P, PA, PB, Q, QB, PQ, S, LA]
    ): Dag[PQ, PA, (PB, QB), S, LA] = dag

    def grada[
      P, PA : AdditiveSemigroup, PB
    , Q, QB
    , PQ
    ](
      dag: Prod.Diff[P, PA, PB, Q, QB, PQ, S, LA]
    ): Dag[PQ, (PA, (PB, QB)), PA, S, LA] = {
      implicit val m = dag.merger
      val gf = grada0(dag.f)
      val gg = grada0(dag.g)
      val r = gf || gg
      // quite ugly to write for now ....
      val init = ((((
        ((id[PA] || (id[PB] || id[QB])) >>> (split[PA, PA, PA] || (id[PB] || id[QB]))) >>> assocL
      ) >>> (flip || id)) >>> (assocL || id)) >>> assocR) >>> (flip || id)
      (init >>> (gf || gg)) >>> join
    }

    def gradp[
      P, PA : AdditiveSemigroup, PB
    , Q, QB
    , PQ
    ](
      dag: Prod.Diff[P, PA, PB, Q, QB, PQ, S, LA]
    ): Dag[PQ, (PA, (PB, QB)), PQ, S, LA] = {
      implicit val m = dag.merger
      val gf = gradp0(dag.f)
      val gg = gradp0(dag.g)
      val r = gf || gg
      // quite ugly to write for now ....
      val init = ((((
        ((id[PA] || (id[PB] || id[QB])) >>> (split[PA, PA, PA] || (id[PB] || id[QB]))) >>> assocL
      ) >>> (flip || id)) >>> (assocL || id)) >>> assocR) >>> (flip || id)
      (init >>> (gf || gg)) >>> join
    }

    def compile[P, A, B](
        dag: Apply[P, A, B, S, LA]
    ): Dag[P, (Dag[P, A, B, S, LA], A), B, S, LA] = {
      dag
    }

//////////////////////////////////////////////////////////////////
// GENERIC
    def compile0[
      P, A, B
    ](dag: Dag[P, A, B, S, LA]): Dag[P, A, B, S, LA] = dag

    def grada0[
      P, A, B
    ](dag: DiffDag[P, A, B, S, LA]): Dag[P, (A, B), A, S, LA] = {
      // for others, it will be the following
      val idA = id[A]
      val idB = id[B]

      val implement = dag.compile[Out](self)
      val err = (implement || idB) >>> dag.costDiff()

      val grada = (idA || err) >>> dag.gradA
      // multiply by norm

      val init = (fst[A, B] ** (((idA || idB) >>> (split[A, A, A] || idB)) >>> assocR[A, A, B]))

      init >>> ((idA || grada) >>> dag.costDiffInvert())
    }

    def gradp0[
      P, A, B
    ](dag: DiffDag[P, A, B, S, LA]): Dag[P, (A, B), P, S, LA] = {
      val idA = id[A]
      val idB = id[B]

      val implement = dag.compile[Out](self)
      val err = (implement || idB) >>> dag.costDiff()
      val gradp = (idA || err) >>> dag.gradP

      val e = ((const(eps) || gradp) >>> dag.scalarTimes()) >>> dag.minusP()

      val init = (split[A, A, A] || idB) >>> assocR[A, A, B]

      init >>> gradp
    }
  }


//   class LearnCompiler[S](eps: S) extends LearnAlgebra[S, Lambda[(p, a, b) => Dag[p, a, b, Lambda[out[p, a, b] => LearnAlgebra[S, out]]]]] {
//     self =>

//     type LA[out[p, a, b]] = LearnAlgebra[S, out]

//     val dsl0 = new DagDsl[LA] {}
//     import dsl0._

//     implicit def aCo[A] = new Comonoid[A] {
//       def split(a: A): (A, A) = (a, a)
//       def destroy(a: A) = ()
//     }

//     type Out[p, a, b] = Dag[p, a, b, LA]

//     def compile[
//       P, PA, PB
//     , Q, QC
//     , PQ
//     ](
//       dag: Compose[P, PA, PB, Q, QC, PQ, LA]
//     ): Dag[PQ, PA, QC, LA] = dag

//     def gradp[
//       P, PA, PB
//     , Q, QC
//     , PQ
//     ](
//       dag: Compose.Diff[P, PA, PB, Q, QC, PQ, S, LA]
//     ): Dag[PQ, (PA, QC), PQ, LA] = {
//       implicit val m = dag.merger

//       val idQC = id[QC]
//       val idPA = id[PA]

//       val pfi = dag.f.compile[Out](self)
//       val pfu = dag.f.gradP.compile[Out](self)
//       val pgu = dag.g.gradP.compile[Out](self)
//       val pgr = dag.g.gradA.compile[Out](self)

//       val pgr2 = (pfi || idQC) >>> pgr
//       val u = ((split[PA] || idQC) >>> assocR[PA, PA, QC]) >>> ((idPA || pgr2) >>> pfu)

//       val v = (pfi || idQC) >>> pgu
//       (u ** v) >>> join
//     }

//     def grada[
//       P, PA, PB
//     , Q, QC
//     , PQ
//     ](
//       dag: Compose.Diff[P, PA, PB, Q, QC, PQ, S, LA]
//     ): Dag[PQ, (PA, QC), PA, LA] = {
//       implicit val m = dag.merger

//       val idQC = id[QC]
//       val idPA = id[PA]

//       val pfi = dag.f.compile[Out](self)
//       val pfr = dag.f.gradA.compile[Out](self)
//       val pgr = dag.g.gradA.compile[Out](self)
//       val reqCompose = (pfi || idQC) >>> pgr

//       val init = ((split[PA] || idQC) >>> assocR[PA, PA, QC])

//       init >>> ((idPA || reqCompose) >>> pfr)
//     }


// //////////////////////////////////////////////////////////////////
// // ID
//     def compile[A](
//       dag: Id[A, LA]
//     ): Dag[HNil, A, A, LA] =
//       compile0(dag)

//     def gradp[A](
//       dag: Id.Diff[A, S, LA]
//     ): Dag[HNil, (A, A), HNil, LA] =
//       gradp0(dag)

//     def grada[A](
//       dag: Id.Diff[A, S, LA]
//     ): Dag[HNil, (A, A), A, LA] =
//       grada0(dag)

// //////////////////////////////////////////////////////////////////
// // CONST
//     def compile[A](
//       dag: Const[A, LA]
//     ): Dag[HNil, Unit, A, LA] =
//       compile0(dag)

//     def grada[A](
//       dag: Const.Diff[A, S, LA]
//     ): Dag[HNil, (Unit, A), Unit, LA] = 
//       grada0(dag)

//     def gradp[A](
//       dag: Const.Diff[A, S, LA]
//     ): Dag[HNil, (Unit, A), HNil, LA] =
//       gradp0(dag)


// //////////////////////////////////////////////////////////////////
// // FST
//     def compile[A, B](
//       dag: Fst[A, B, LA]
//     ): Dag[HNil, (A, B), A, LA] =
//       compile0(dag)

//     def grada[A, B : Zeroed](
//       dag: Fst.Diff[A, B, S, LA]
//     ): Dag[HNil, ((A, B), A), (A, B), LA] =
//       grada0(dag)

//     def gradp[A, B](
//       dag: Fst.Diff[A, B, S, LA]
//     ): Dag[HNil, ((A, B), A), HNil, LA] =
//       gradp0(dag)

// //////////////////////////////////////////////////////////////////
// // SND
//     def compile[A, B](
//       dag: Snd[A, B, LA]
//     ): Dag[HNil, (A, B), B, LA] =
//       compile0(dag)

//     def grada[A : Zeroed, B](
//       dag: Snd.Diff[A, B, S, LA]
//     ): Dag[HNil, ((A, B), B), (A, B), LA] =
//       grada0(dag)

//     def gradp[A, B](
//       dag: Snd.Diff[A, B, S, LA]
//     ): Dag[HNil, ((A, B), B), HNil, LA] =
//       gradp0(dag)

// //////////////////////////////////////////////////////////////////
// // JOIN
//     def compile[A, B, C](
//       dag: Join[A, B, C, LA]
//     ): Dag[HNil, (A, B), C, LA] =
//       compile0(dag)

//     def grada[A, B, C](
//       dag: Join.Diff[A, B, C, S, LA]
//     ): Dag[HNil, ((A, B), C), (A, B), LA] = 
//       grada0(dag)

//     def gradp[A, B, C](
//       dag: Join.Diff[A, B, C, S, LA]
//     ): Dag[HNil, ((A, B), C), HNil, LA] =
//       gradp0(dag)

// //////////////////////////////////////////////////////////////////
// // SPLIT
//     def compile[A : Comonoid](
//       dag: Split[A, LA]
//     ): Dag[HNil, A, (A, A), LA] =
//       compile0(dag)

//     def grada[A : AdditiveSemigroup](
//       dag: Split.Diff[A, S, LA]
//     ): Dag[HNil, (A, (A, A)), A, LA] =
//       grada0(dag)

//     def gradp[A](
//       dag: Split.Diff[A, S, LA]
//     ): Dag[HNil, (A, (A, A)), HNil, LA] =
//       gradp0(dag)

// //////////////////////////////////////////////////////////////////
// // PROD
//     def compile[
//       P, PA, PB
//     , Q, QB
//     , PQ
//     ](
//       dag: Prod[P, PA, PB, Q, QB, PQ, LA]
//     ): Dag[PQ, PA, (PB, QB), LA] = dag

//     def grada[
//       P, PA : AdditiveSemigroup, PB
//     , Q, QB
//     , PQ
//     ](
//       dag: Prod.Diff[P, PA, PB, Q, QB, PQ, S, LA]
//     ): Dag[PQ, (PA, (PB, QB)), PA, LA] = {
//       implicit val m = dag.merger
//       val gf = grada0(dag.f)
//       val gg = grada0(dag.g)
//       val r = gf || gg
//       // quite ugly to write for now ....
//       val init = ((((
//         ((id[PA] || (id[PB] || id[QB])) >>> (split[PA] || (id[PB] || id[QB]))) >>> assocL
//       ) >>> (flip || id)) >>> (assocL || id)) >>> assocR) >>> (flip || id)
//       (init >>> (gf || gg)) >>> join
//     }

//     def gradp[
//       P, PA : AdditiveSemigroup, PB
//     , Q, QB
//     , PQ
//     ](
//       dag: Prod.Diff[P, PA, PB, Q, QB, PQ, S, LA]
//     ): Dag[PQ, (PA, (PB, QB)), PQ, LA] = {
//       implicit val m = dag.merger
//       val gf = gradp0(dag.f)
//       val gg = gradp0(dag.g)
//       val r = gf || gg
//       // quite ugly to write for now ....
//       val init = ((((
//         ((id[PA] || (id[PB] || id[QB])) >>> (split[PA] || (id[PB] || id[QB]))) >>> assocL
//       ) >>> (flip || id)) >>> (assocL || id)) >>> assocR) >>> (flip || id)
//       (init >>> (gf || gg)) >>> join
//     }

//     def compile[P, A, B](
//         dag: Apply[P, A, B, LA]
//     ): Dag[P, (Dag[P, A, B, LA], A), B, LA] = {
//       dag
//     }

// //////////////////////////////////////////////////////////////////
// // GENERIC
//     def compile0[
//       P, A, B
//     ](dag: Dag[P, A, B, LA]): Dag[P, A, B, LA] = dag

//     def grada0[
//       P, A, B
//     ](dag: DiffDag[P, A, B, S, LA]): Dag[P, (A, B), A, LA] = {
//       // for others, it will be the following
//       val idA = id[A]
//       val idB = id[B]

//       val implement = dag.compile[Out](self)
//       val err = (implement || idB) >>> dag.costDiff()

//       val grada = (idA || err) >>> dag.gradA
//       // multiply by norm

//       val init = (fst[A, B] ** (((idA || idB) >>> (split[A]() || idB)) >>> assocR[A, A, B]))

//       init >>> ((idA || grada) >>> dag.costDiffInvert())
//     }

//     def gradp0[
//       P, A, B
//     ](dag: DiffDag[P, A, B, S, LA]): Dag[P, (A, B), P, LA] = {
//       val idA = id[A]
//       val idB = id[B]

//       val implement = dag.compile[Out](self)
//       val err = (implement || idB) >>> dag.costDiff()
//       val gradp = (idA || err) >>> dag.gradP

//       val init = (split[A] || idB) >>> assocR[A, A, B]

//       init >>> gradp
//     }

//   }

//   case class Compiler[
//     S, K[alg[out[p, a, b]] <: DiffDagAlgebra[alg, out, s, K], s] <: Calculus[alg, s, K]
//   ](
//     eps: S, calculus: Calculus[Lambda[out[p, a, b] => LearnAlgebra[out, S, K]], S, K]
//   ) extends LearnAlgebra[Lambda[(p, a, b) => Dag[p, a, b, Lambda[out[p, a, b] => LearnAlgebra[out, S, K]], S, K]], S, K] {
//     self =>


//     type LA[out[p, a, b]] = LearnAlgebra[out, S, K]
    
//     val dsl0 = new DagDsl[LA] {}
//     import dsl0._

//     implicit def aCo[A] = new Comonoid[A] {
//       def split(a: A): (A, A) = (a, a)
//       def destroy(a: A) = ()
//     }

//     type Out[p, a, b] = Dag[p, a, b, LA, S, K]

//     def compile[
//       P, PA, PB
//     , Q, QC
//     , PQ
//     ](
//       dag: Compose[P, PA, PB, Q, QC, PQ, LA, S, K]
//     ): Dag[PQ, PA, QC, LA, S, K] = dag

//     def gradp[
//       P, PA, PB
//     , Q, QC
//     , PQ
//     ](
//       dag: Compose.Diff[P, PA, PB, Q, QC, PQ, LA, S, K]
//     ): Dag[PQ, (PA, QC), PQ, LA, S, K] = {
//       implicit val m = dag.merger

//       val idQC = id[QC]
//       val idPA = id[PA]

//       val pfi = dag.f.compile[Out](self)
//       val pfu = dag.f.gradP.compile[Out](self)
//       val pgu = dag.g.gradP.compile[Out](self)
//       val pgr = dag.g.gradA.compile[Out](self)


//       val pr = pfi || idQC
//       val rrr = new DagOps(pr)
//       val pgr2 = (pfi || idQC) >>> pgr
//       val u = ((split[PA] || idQC) >>> assocR[PA, PA, QC]) >>> ((idPA || pgr2) >>> pfu)

//       val v = (pfi || idQC) >>> pgu
//       (u ** v) >>> join
//     }

//     def grada[
//       P, PA, PB
//     , Q, QC
//     , PQ
//     ](
//       dag: Compose.Diff[P, PA, PB, Q, QC, PQ, LA, S, K]
//     ): Dag[PQ, (PA, QC), PA, LA, S, K] = {
//       implicit val m = dag.merger

//       val idQC = id[QC]
//       val idPA = id[PA]

//       val pfi = dag.f.compile[Out](self)
//       val pfr = dag.f.gradA.compile[Out](self)
//       val pgr = dag.g.gradA.compile[Out](self)
//       val reqCompose = (pfi || idQC) >>> pgr

//       val init = ((split[PA] || idQC) >>> assocR[PA, PA, QC])

//       init >>> ((idPA || reqCompose) >>> pfr)
//     }

// // //////////////////////////////////////////////////////////////////
// // // CONST
// //     def compile[A](
// //       dag: Id[A, LA]
// //     ): Dag[HNil, A, A, LA] =
// //       compile0(dag)

// //     def gradp[A](
// //       dag: Id.Diff[A, LA]
// //     ): Dag[HNil, (A, A), HNil, LA] =
// //       gradp0(dag)

// //     def grada[A](
// //       dag: Id.Diff[A, LA]
// //     ): Dag[HNil, (A, A), A, LA] =
// //       grada0(dag)

// // //////////////////////////////////////////////////////////////////
// // // CONST
// //     def compile[A](
// //       dag: Const[A, LA]
// //     ): Dag[HNil, Unit, A, LA] =
// //       compile0(dag)

// //     def grada[A](
// //       dag: Const.Diff[A, LA]
// //     ): Dag[HNil, (Unit, A), Unit, LA] = 
// //       grada0(dag)

// //     def gradp[A](
// //       dag: Const.Diff[A, LA]
// //     ): Dag[HNil, (Unit, A), HNil, LA] =
// //       gradp0(dag)

// // //////////////////////////////////////////////////////////////////
// // // FST
// //     def compile[A, B](
// //       dag: Fst[A, B, LA]
// //     ): Dag[HNil, (A, B), A, LA] =
// //       compile0(dag)

// //     def grada[A, B : Zeroed](
// //       dag: Fst.Diff[A, B, LA]
// //     ): Dag[HNil, ((A, B), A), (A, B), LA] =
// //       grada0(dag)

// //     def gradp[A, B](
// //       dag: Fst.Diff[A, B, LA]
// //     ): Dag[HNil, ((A, B), A), HNil, LA] =
// //       gradp0(dag)

// // //////////////////////////////////////////////////////////////////
// // // SND
// //     def compile[A, B](
// //       dag: Snd[A, B, LA]
// //     ): Dag[HNil, (A, B), B, LA] =
// //       compile0(dag)

// //     def grada[A : Zeroed, B](
// //       dag: Snd.Diff[A, B, LA]
// //     ): Dag[HNil, ((A, B), B), (A, B), LA] =
// //       grada0(dag)

// //     def gradp[A, B](
// //       dag: Snd.Diff[A, B, LA]
// //     ): Dag[HNil, ((A, B), B), HNil, LA] =
// //       gradp0(dag)

// // //////////////////////////////////////////////////////////////////
// // // Join
// //     def compile[A, B, C](
// //       dag: Join[A, B, C, LA]
// //     ): Dag[HNil, (A, B), C, LA] =
// //       compile0(dag)

// //     def grada[A, B, C](
// //       dag: Join.Diff[A, B, C, LA]
// //     ): Dag[HNil, ((A, B), C), (A, B), LA] = 
// //       grada0(dag)

// //     def gradp[A, B, C](
// //       dag: Join.Diff[A, B, C, LA]
// //     ): Dag[HNil, ((A, B), C), HNil, LA] =
// //       gradp0(dag)

// // //////////////////////////////////////////////////////////////////
// // // SPLIT
// //     def compile[A : Comonoid](
// //       dag: Split[A, LA]
// //     ): Dag[HNil, A, (A, A), LA] =
// //       compile0(dag)

// //     def grada[A : AdditiveSemigroup](
// //       dag: Split.Diff[A, LA]
// //     ): Dag[HNil, (A, (A, A)), A, LA] =
// //       grada0(dag)

// //     def gradp[A](
// //       dag: Split.Diff[A, LA]
// //     ): Dag[HNil, (A, (A, A)), HNil, LA] =
// //       gradp0(dag)

// // //////////////////////////////////////////////////////////////////
// // // PROD
// //     def compile[
// //       P, PA, PB
// //     , Q, QB
// //     , PQ
// //     ](
// //       dag: Prod[P, PA, PB, Q, QB, PQ, LA]
// //     ): Dag[PQ, PA, (PB, QB), LA] = dag

// //     def grada[
// //       P, PA : AdditiveSemigroup, PB
// //     , Q, QB
// //     , PQ
// //     ](
// //       dag: Prod.Diff[P, PA, PB, Q, QB, PQ, LA]
// //     ): Dag[PQ, (PA, (PB, QB)), PA, LA] = {
// //       implicit val m = dag.merger
// //       val gf = grada0(dag.f)
// //       val gg = grada0(dag.g)
// //       val r = gf || gg
// //       // quite ugly to write for now ....
// //       val init = ((((
// //         ((id[PA] || (id[PB] || id[QB])) >>> (split[PA] || (id[PB] || id[QB]))) >>> assocL
// //       ) >>> (flip || id)) >>> (assocL || id)) >>> assocR) >>> (flip || id)
// //       (init >>> (gf || gg)) >>> join
// //     }

// //     def gradp[
// //       P, PA : AdditiveSemigroup, PB
// //     , Q, QB
// //     , PQ
// //     ](
// //       dag: Prod.Diff[P, PA, PB, Q, QB, PQ, LA]
// //     ): Dag[PQ, (PA, (PB, QB)), PQ, LA] = {
// //       implicit val m = dag.merger
// //       val gf = gradp0(dag.f)
// //       val gg = gradp0(dag.g)
// //       val r = gf || gg
// //       // quite ugly to write for now ....
// //       val init = ((((
// //         ((id[PA] || (id[PB] || id[QB])) >>> (split[PA] || (id[PB] || id[QB]))) >>> assocL
// //       ) >>> (flip || id)) >>> (assocL || id)) >>> assocR) >>> (flip || id)
// //       (init >>> (gf || gg)) >>> join
// //     }


// // //////////////////////////////////////////////////////////////////
// // // GENERIC
//     def compile0[
//       P, A, B
//     ](dag: Dag[P, A, B, LA, S, K]): Dag[P, A, B, LA, S, K] = dag


//     // def grada0[
//     //   P, A, B
//     // , S, K[alg[out[p, a, b]] <: DiffDagAlgebra[alg, out], s] <: Calculus[alg, s, K]
//     // ](dag: DiffDag[P, A, B, LA, S, K]): Dag[P, (A, B), A, LA] = {
//     //   // for others, it will be the following
//     //   val idA = id[A]
//     //   val idB = id[B]

//     //   val implement = dag.compile[Out](self)
//     //   val err = (implement || idB) >>> dag.costB //cost.diff[B]

//     //   val grada = (idA || err) >>> dag.gradA
//     //   // multiply by norm

//     //   val init = (fst[A, B] ** (((idA || idB) >>> (split[A]() || idB)) >>> assocR[A, A, B]))

//     //   init >>> ((idA || grada) >>> costDiffInvert[A])
//     // }


// //     def gradp0[
// //       P, A, B
// //     ](dag: DiffDag[P, A, B, LA]): Dag[P, (A, B), P, LA] = {
// //       val idA = id[A]
// //       val idB = id[B]

// //       val implement = dag.compile[Out](self)
// //       val err = (implement || idB) >>> costDiff[B]
// //       val gradp = (idA || err) >>> dag.gradP

// //       val init = (split[A] || idB) >>> assocR[A, A, B]

// //       init >>> gradp
// //     }

// //   }

// // }


