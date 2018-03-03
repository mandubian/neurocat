package neurocat
package idag

import shapeless.HNil


class Apply[P, A, B, S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out]]
extends Dag[P, (Dag[P, A, B, S, Alg], A), B, S, Alg] {
  self =>

  def compile[Out[p, a, b]](
    compiler: Alg[Out]
  ): Out[P, (Dag[P, A, B, S, Alg], A), B] = compiler.compile(self)

}


object Apply {

  trait Algebra[S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out], Out[p, a, b]] {

    def compile[P, A, B](
      dag: Apply[P, A, B, S, Alg]
    ): Out[P, (Dag[P, A, B, S, Alg], A), B]
  }


  trait Dsl[S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out]] {

    def ap[P, A, B]: Dag[P, (Dag[P, A, B, S, Alg], A), B, S, Alg] = new Apply()
  }

}

