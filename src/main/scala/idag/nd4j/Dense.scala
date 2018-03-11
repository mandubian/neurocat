package neurocat
package idag
package nd4j


import shapeless.{::, HNil}
import typeclasses._
import neurocat.nd4j._
import singleton.ops._
import shapeless.labelled._


class Dense[
  S, InR <: XInt, OutR <: XInt, OutC <: XInt
, Alg[out[p, a, b]] <: Dense.Algebra[S, Alg, out]
]() extends Dag[
    Mat[S, OutR x InR] :: HNil
  , Mat[S, InR x OutC]
  , Mat[S, OutR x OutC]
  , S, Alg
  ] {
    self =>
    def compile[Out[p, a, b]](
      compiler: Alg[Out]
    ): Out[
      Mat[S, OutR x InR] :: HNil
    , Mat[S, InR x OutC]
    , Mat[S, OutR x OutC]
    ] = compiler.compile(self)
  }

object Dense {


  abstract class Diff[
    S, InR <: XInt, OutR <: XInt, OutC <: XInt
  , Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out]
  ]()
  extends Dense[S, InR, OutR, OutC, Alg] with DiffDag[
    Mat[S, OutR x InR] :: HNil
  , Mat[S, InR x OutC]
  , Mat[S, OutR x OutC]
  , S, Alg
  ] {
    self =>

    def gradA = new Dag[Mat[S, OutR x InR] :: HNil, (Mat[S, InR x OutC], Mat[S, OutR x OutC]), Mat[S, InR x OutC], S, Alg] {
        def compile[Out[p, a, b]](
          compiler: Alg[Out]
        ): Out[Mat[S, OutR x InR] :: HNil, (Mat[S, InR x OutC], Mat[S, OutR x OutC]), Mat[S, InR x OutC]] = compiler.grada(self)
      }

    def gradP = new Dag[Mat[S, OutR x InR] :: HNil, (Mat[S, InR x OutC], Mat[S, OutR x OutC]), Mat[S, OutR x InR] :: HNil, S, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[Mat[S, OutR x InR] :: HNil, (Mat[S, InR x OutC], Mat[S, OutR x OutC]), Mat[S, OutR x InR] :: HNil] = compiler.gradp(self)
    }

  }

  trait Algebra[S, Alg[out[p, a, b]] <: Algebra[S, Alg, out], Out[p, a, b]]
  extends DagAlgebra[S, Alg, Out] {

    def compile[InR <: XInt, OutR <: XInt, OutC <: XInt](
      dag: Dense[S, InR, OutR, OutC, Alg]
    ): Out[
      Mat[S, OutR x InR] :: HNil
    , Mat[S, InR x OutC]
    , Mat[S, OutR x OutC]
    ]

  }


  trait DiffAlgebra[
    S, Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out], Out[p, a, b]
  ] extends Algebra[S, Alg, Out] {

    def grada[
      InR <: XInt, OutR <: XInt, OutC <: XInt
    ](
      dag: Dense.Diff[S, InR, OutR, OutC, Alg]
    ): Out[Mat[S, OutR x InR] :: HNil, (Mat[S, InR x OutC], Mat[S, OutR x OutC]), Mat[S, InR x OutC]]

    def gradp[InR <: XInt, OutR <: XInt, OutC <: XInt](
      dag: Dense.Diff[S, InR, OutR, OutC, Alg]
    ): Out[Mat[S, OutR x InR] :: HNil, (Mat[S, InR x OutC], Mat[S, OutR x OutC]), Mat[S, OutR x InR] :: HNil]
  }


  trait Dsl[S, Alg[out[p, a, b]] <: nd4j.Algebra[S, Alg, out]] {

    def dense[In <: Dim2[_, _], Out <: Dim2[_, _]](
      implicit same: Dim2SameCol[In, Out]
    ): Dag[
        Mat[S, same.OutR x same.In] :: HNil
      , Mat[S, same.In x same.OutC]
      , Mat[S, same.OutR x same.OutC]
      , S, Alg
    ] = new Dense()

  }  


  trait DiffDsl[
    S, Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out]
  ] {

    def dense[InR <: XInt, OutC <: XInt, OutR <: XInt](
      implicit
        costIn: CostDiffInvertBuilder[Mat[S, InR x OutC], S, Alg]
      , costOut: CostDiffBuilder[Mat[S, OutR x OutC], S, Alg]
      // , scalarTimesOut: ScalarTimesBuilder[Mat[S, OutR x InR] :: HNil, S, Alg]
      // , minusA0: MinusBuilder[Mat[S, InR x OutC], S, Alg]
      // , minusP0: MinusPBuilder[Mat[S, OutR x InR] :: HNil, S, Alg]
    ): DiffDag[
        Mat[S, OutR x InR] :: HNil
      , Mat[S, InR x OutC]
      , Mat[S, OutR x OutC]
      , S, Alg
    ] = new Dense.Diff[S, InR, OutR, OutC, Alg] {
      val costDiffInvert = costIn
      val costDiff = costOut
      val scalarTimes = implicitly[ScalarTimesBuilder[Mat[S, OutR x InR] :: HNil, S, Alg]] //scalarTimesOut
      val minusA = implicitly[MinusBuilder[Mat[S, InR x OutC], S, Alg]] //minusA0
      val minusP = implicitly[MinusPBuilder[Mat[S, OutR x InR] :: HNil, S, Alg]] //minusP0
    }
  }

}