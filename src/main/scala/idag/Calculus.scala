package neurocat
package idag

import shapeless.HNil
import shapeless.labelled.FieldType
import typeclasses._


trait ScalarTimesBuilder[
  A, S
, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
] {
  def apply(): Dag[HNil, (S, A), A, S, Alg]
}


object ScalarTimesBuilder extends ScalarTimesBuilder1 {
  // implicit def aCo[A] = new Comonoid[A] {
  //     def split(a: A): (A, A) = (a, a)
  //     def destroy(a: A) = ()
  //   }

  implicit def hnil[
    S
  , Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ] = new ScalarTimesBuilder[HNil, S, Alg] {
    val dsl = new DagDsl[S, Alg] {}
    import dsl._
    def apply() = snd[S, HNil]

  }    

  implicit def tuple[
    A, B, S
  , Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ](
    implicit ca: ScalarTimesBuilder[A, S, Alg], cb: ScalarTimesBuilder[B, S, Alg]
  ) = new ScalarTimesBuilder[(A, B), S, Alg] {
    val dsl = new DagDsl[S, Alg] {}
    import dsl._
    def apply() = {

      val init = ((((split[S, S, S] || (id[A] || id[B])) >>> assocR) >>> (id[S] || (assocL >>> flip))) >>> assocL) >>> flip
        //((((((split[S] || id[A]) >>> assocR) >>> (id[S] || flip)) >>> assocL) || id[B]) >>> assocR
      init >>> (ca() || cb())
    }

  }
}

trait ScalarTimesBuilder1 {

  implicit def merger[
    P, Q, S, PQ
  , Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ](implicit
    merger: Merger.Aux[P, Q, PQ]
  , mp: ScalarTimesBuilder[P, S, Alg]
  , mq: ScalarTimesBuilder[Q, S, Alg]
  ): ScalarTimesBuilder[PQ, S, Alg] = {
    val dsl = new DagDsl[S, Alg] {}
    import dsl._
    new ScalarTimesBuilder[PQ, S, Alg] {
      def apply(): Dag[HNil, (S, PQ), PQ, S, Alg] = {
        (((split[S, S, S] || split[PQ, P, Q]) >>> interleave[S, S, P, Q]) >>>
          (mp() || mq())) >>> join[P, Q, PQ]
      }
    }
  }

}

trait MinusBuilder[
  A, S
, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
] {
  def apply(): Dag[HNil, (A, A), A, S, Alg]
}

object MinusBuilder {
  implicit def aCo[A] = new Comonoid[A] {
      def split(a: A): (A, A) = (a, a)
      def destroy(a: A) = ()
    }

  implicit def tuple[
    A, B, S
  , Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ](
    implicit ca: MinusBuilder[A, S, Alg], cb: MinusBuilder[B, S, Alg]
  ) = new MinusBuilder[(A, B), S, Alg] {
    val dsl = new DagDsl[S, Alg] {}
    import dsl._
    def apply() = {
      val init = ((((id[A] || id[B]) || (id[A] || id[B])) >>> assocR) >>> (id[A] || (flip >>> assocR))) >>> assocL
      init >>> (ca() || cb())
    }

  }
}

trait MinusPBuilder[
  P, S
, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
] {
  def apply(): Dag[P, P, P, S, Alg]
}

object MinusPBuilder extends MinusPBuilder1 {
  implicit def hnil[
    S
  , Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ] = new MinusPBuilder[HNil, S, Alg] {
    val dsl = new DagDsl[S, Alg] {}
    import dsl._
    def apply() = id[HNil]

  }
}

trait MinusPBuilder1 {

  implicit def m[
    P, Q, S
  , Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  , PQ
  ](implicit merger: Merger.Aux[P, Q, PQ], mp: MinusPBuilder[P, S, Alg], mq: MinusPBuilder[Q, S, Alg]) = {
    val dsl = new DagDsl[S, Alg] {}
    import dsl._
    new MinusPBuilder[PQ, S, Alg] {
      def apply(): Dag[PQ, PQ, PQ, S, Alg] = {
        slideL((((split[PQ, P, Q] || split[PQ, P, Q]) >>> interleave[P, Q, P, Q]) >>> (slideR(mp()) || slideR(mq()))) >>> join)
      }
    }
  }
}



trait CostBuilder[
  A, S
, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
] {
  def apply(): Dag[HNil, (A, A), A, S, Alg]
}

object CostBuilder {

  implicit def aCo[A] = new Comonoid[A] {
      def split(a: A): (A, A) = (a, a)
      def destroy(a: A) = ()
    }

  implicit def costTuple[
    A, B, S
  , Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ](
    implicit ca: CostBuilder[A, S, Alg], cb: CostBuilder[B, S, Alg]
  ) = new CostBuilder[(A, B), S, Alg] {
    val dsl = new DagDsl[S, Alg] {}
    import dsl._
    def apply() = {
      val init = ((id[A] || id[B]) || (id[A] || id[B])) >>> assocR >>> (id[A] || flip) >>> (id[A] || assocR)
      init >>> assocL >>> (par(ca(), cb()))
    }

  }
}

trait CostDiffBuilder[
  A, S
, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
] {
  def apply(): Dag[HNil, (A, A), A, S, Alg]
}

object CostDiffBuilder {
  implicit def costTuple[
    A, B, S
  , Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ](
    implicit ca: CostDiffBuilder[A, S, Alg], cb: CostDiffBuilder[B, S, Alg]
  ) = new CostDiffBuilder[(A, B), S, Alg] {
    val dsl = new DagDsl[S, Alg] {}
    import dsl._
    def apply() = {
      val init = ((id[A] || id[B]) || (id[A] || id[B])) >>> assocR >>> (id[A] || flip) >>> (id[A] || assocR)
      init >>> assocL >>> (par(ca(), cb()))
    }

  }  
}

trait CostDiffInvertBuilder[
  A, S
, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
] {
  def apply(): Dag[HNil, (A, A), A, S, Alg]
}

object CostDiffInvertBuilder {
  implicit def costTuple[
    A, B, S
  , Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out]
  ](
    implicit ca: CostDiffInvertBuilder[A, S, Alg], cb: CostDiffInvertBuilder[B, S, Alg]
  ) = new CostDiffInvertBuilder[(A, B), S, Alg] {
    val dsl = new DagDsl[S, Alg] {}
    import dsl._
    def apply() = {
      val init = ((id[A] || id[B]) || (id[A] || id[B])) >>> assocR >>> (id[A] || flip) >>> (id[A] || assocR)
      init >>> assocL >>> (par(ca(), cb()))
    }

  }  
}

