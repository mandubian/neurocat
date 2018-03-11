package neurocat
package idag

import neurocat.nd4j._
import shapeless.{HNil, ::, HList}
import shapeless.labelled._

package object nd4j {

  implicit def matMinus[S, D <: Dim, Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out]] =
    new MinusBuilder[Mat[S, D], S, Alg] {
      def apply(): Dag[HNil, (Mat[S, D], Mat[S, D]), Mat[S, D], S, Alg] = new Minus()
    }

  implicit def matMinusP[S, D <: Dim, Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out]] =
    new MinusPBuilder[Mat[S, D], S, Alg] {
      def apply(): Dag[Mat[S, D], Mat[S, D], Mat[S, D], S, Alg] = new SlideL(new Minus())
    }

  implicit def matScalarTimesBuilder[S, D <: Dim, Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out]] =
    new ScalarTimesBuilder[Mat[S, D], S, Alg] {
      def apply(): Dag[HNil, (S, Mat[S, D]), Mat[S, D], S, Alg] = new ScalarTimes()
    }



  implicit def scalarTimesBuilderCons[A, B <: HList, S, Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out]](
    implicit sca: ScalarTimesBuilder[A, S, Alg], scb: ScalarTimesBuilder[B, S, Alg]
  ) = {
    val dsl = new DagDsl[S, Alg] with nd4j.Dsl[S, Alg] {}
    import dsl._

    new ScalarTimesBuilder[A :: B, S, Alg] {
      def apply(): Dag[HNil, (S, A :: B), A :: B, S, Alg] = {
        ((id[(S, A :: B)] >>> func({ case (s, a) => ((s, a.head), (s, a.tail)) })) >>>
        (sca() || scb())) >>> func({ case (a, b) => a :: b })
      }
    }
  }

  implicit def scalarTimesBuilderFieldType[Sym <: Singleton, A, S, Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out]](
    implicit sc: ScalarTimesBuilder[A, S, Alg]
  ) = {
    val dsl = new DagDsl[S, Alg] with nd4j.Dsl[S, Alg] {}
    import dsl._

    new ScalarTimesBuilder[FieldType[Sym, A], S, Alg] {
      def apply(): Dag[HNil, (S, FieldType[Sym, A]), FieldType[Sym, A], S, Alg] = {
        ((id[(S, FieldType[Sym, A])] >>> func({ case (s, a) => s -> (a:A) })) >>>
          sc()) >>> func({ (a: A) => shapeless.labelled.field[Sym](a) })
      }
    }
  }

  implicit def minusPBuilderCons[A, B <: HList, S, Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out]](
    implicit mpa: MinusPBuilder[A, S, Alg], mpb: MinusPBuilder[B, S, Alg]
  ) = {
    val dsl = new DagDsl[S, Alg] with nd4j.Dsl[S, Alg] {}
    import dsl._

    new MinusPBuilder[A :: B, S, Alg] {
      def apply(): Dag[A :: B, A :: B, A :: B, S, Alg] = {
        slideL(((id[(A :: B, A :: B)] >>> func({ case (a1, a2) => ((a1.head, a2.head), (a1.tail, a2.tail)) })) >>>
          (slideR(mpa()) || slideR(mpb()))) >>> func({ case (a, b) => a :: b }))
      }
    }
  }

  implicit def minusPBuilderFieldType[Sym <: Singleton, A, S, Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out]](
    implicit mp: MinusPBuilder[A, S, Alg]
  ) = {
    val dsl = new DagDsl[S, Alg] with nd4j.Dsl[S, Alg] {}
    import dsl._

    new MinusPBuilder[FieldType[Sym, A], S, Alg] {
      def apply(): Dag[FieldType[Sym, A], FieldType[Sym, A], FieldType[Sym, A], S, Alg] = {
        slideL(((id[(FieldType[Sym, A], FieldType[Sym, A])] >>> func({ case (a1, a2) => (a1:A) -> (a2:A) })) >>>
          slideR(mp())) >>> func({ (a: A) => shapeless.labelled.field[Sym](a) }))
      }
    }
  }

  // implicit def minusPBuilderHNil[A, S, Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out]](
  //   implicit mp: MinusPBuilder[A, S, Alg]
  // ) = {
  //   val dsl = new DagDsl[S, Alg] with nd4j.Dsl[S, Alg] {}
  //   import dsl._

  //   new MinusPBuilder[HNil, S, Alg] {
  //     def apply(): Dag[HNil, HNil, HNil, S, Alg] = func({ a => HNil })
  //   }
  // }

}

