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
      val u = ((split[PA, PA] || idQC) >>> assocR[PA, PA, QC]) >>> ((idPA || pgr2) >>> pfu)

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

      val init = ((split[PA, PA] || idQC) >>> assocR[PA, PA, QC])

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
// SLIDEL
    def compile[P, A, B](
      dag: SlideL[P, A, B, S, LA]
    ): Dag[P, A, B, S, LA] =
      compile0(dag)

    def grada[P, A, B](
      dag: SlideL.Diff[P, A, B, S, LA]
    ): Dag[P, (A, B), A, S, LA] =
      grada0(dag)

    def gradp[P, A, B](
      dag: SlideL.Diff[P, A, B, S, LA]
    ): Dag[P, (A, B), P, S, LA] =
      gradp0(dag)

//////////////////////////////////////////////////////////////////
// SLIDER
    def compile[P, A, B](
      dag: SlideR[P, A, B, S, LA]
    ): Dag[HNil, (P, A), B, S, LA] =
      compile0(dag)

    def grada[P, A, B](
      dag: SlideR.Diff[P, A, B, S, LA]
    ): Dag[HNil, ((P, A), B), (P, A), S, LA] =
      grada0(dag)

    def gradp[P, A, B](
      dag: SlideR.Diff[P, A, B, S, LA]
    ): Dag[HNil, ((P, A), B), HNil, S, LA] =
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
        ((id[PA] || (id[PB] || id[QB])) >>> (split[PA, PA] || (id[PB] || id[QB]))) >>> assocL
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
        ((id[PA] || (id[PB] || id[QB])) >>> (split[PA, PA] || (id[PB] || id[QB]))) >>> assocL
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

      val init = (fst[A, B] ** (((idA || idB) >>> (split[A, A] || idB)) >>> assocR[A, A, B]))

      init >>> ((idA || grada) >>> dag.costDiffInvert())
    }


    implicit def mergerUnit[A] = new Merger[Unit, A] {
      type Out = A
      def left(a: A): Unit = ()
      def right(a: A): A = a
      def apply(u: Unit, a: A): A = a
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

      val init = ((split[Unit, A] || id[B]) >>> assocR) >>> (id[Unit] || ((split[A, A] || id[B]) >>> assocR))

      init >>> e
    }
  }



