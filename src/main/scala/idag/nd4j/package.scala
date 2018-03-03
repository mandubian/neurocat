package neurocat
package idag

import neurocat.nd4j._
import shapeless.HNil
import shapeless.labelled._

package object nd4j {

  implicit def minus[S, D <: Dim, Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out]] =
    new MinusBuilder[Mat[S, D], S, Alg] {
      def apply(): Dag[HNil, (Mat[S, D], Mat[S, D]), Mat[S, D], S, Alg] = new Minus()
    }


  implicit def field[Sym <: Singleton, A, S, Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out]](
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

  // implicit def hnil[A, S, Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out]](
  //   implicit sc: ScalarTimesBuilder[A, S, Alg]
  // ) = new ScalarTimesBuilder[A :: HNil, S, Alg] {
  //   def apply(): Dag[HNil, (S, A :: HNil), A :: HNil, S, Alg] = {
  //     sc() >>> (new JavaFun({ (s, a) => s -> shapeless.labelled.field[Sym](a) }))
  //   }
  // }

}

