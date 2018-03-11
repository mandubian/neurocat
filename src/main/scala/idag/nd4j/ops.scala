package neurocat
package idag
package nd4j


import shapeless.{::, HNil}
import typeclasses._
import neurocat.nd4j._



class ScalarTimes[
  S, D <: Dim
, Alg[out[p, a, b]] <: nd4j.Algebra[S, Alg, out]
] extends Dag[HNil, (S, Mat[S, D]), Mat[S, D], S, Alg] {
  self =>
  def compile[Out[p, a, b]](
    compiler: Alg[Out]
  ): Out[HNil, (S, Mat[S, D]), Mat[S, D]] =
    compiler.compile(self)
}

object ScalarTimes {

  trait Algebra[S, Alg[out[p, a, b]] <: nd4j.Algebra[S, Alg, out], Out[p, a, b]]
  extends DagAlgebra[S, Alg, Out] {

    def compile[D <: Dim](dag: ScalarTimes[S, D, Alg]): Out[HNil, (S, Mat[S, D]), Mat[S, D]]

  }

}



class Minus[
  D <: Dim
, S, Alg[out[p, a, b]] <: nd4j.Algebra[S, Alg, out]
] extends Dag[HNil, (Mat[S, D], Mat[S, D]), Mat[S, D], S, Alg] {
  self =>
  def compile[Out[p, a, b]](
    compiler: Alg[Out]
  ): Out[HNil, (Mat[S, D], Mat[S, D]), Mat[S, D]] =
    compiler.compile(self)
}

object Minus {

  trait Algebra[S, Alg[out[p, a, b]] <: nd4j.Algebra[S, Alg, out], Out[p, a, b]]
  extends DagAlgebra[S, Alg, Out] {

    def compile[D <: Dim](dag: Minus[D, S, Alg]): Out[HNil, (Mat[S, D], Mat[S, D]), Mat[S, D]]

  }

}

class Func[
  A, B
, S, Alg[out[p, a, b]] <: nd4j.Algebra[S, Alg, out]
](val f: A => B) extends Dag[HNil, A, B, S, Alg] {
  self =>
  def compile[Out[p, a, b]](
    compiler: Alg[Out]
  ): Out[HNil, A, B] =
    compiler.compile(self)
}

object Func {

  trait Algebra[S, Alg[out[p, a, b]] <: nd4j.Algebra[S, Alg, out], Out[p, a, b]]
  extends DagAlgebra[S, Alg, Out] {

    def compile[A, B](dag: Func[A, B, S, Alg]): Out[HNil, A, B]

  }

  trait Dsl[S, Alg[out[p, a, b]] <: nd4j.Algebra[S, Alg, out]] {

    def func[A, B](
      f: A => B
    ): Dag[
        HNil, A, B
      , S, Alg
    ] = new Func(f)

  }  
}


