package neurocat
package idag
package nd4j


import shapeless.{::, HNil}
import typeclasses._
import neurocat.nd4j._
import singleton.ops._
import shapeless.labelled._


class WeightMat[
  Sym <: Singleton, S, InR <: XInt, OutR <: XInt, OutC <: XInt
, Alg[out[p, a, b]] <: WeightMat.Algebra[S, Alg, out]
]() extends Dag[
    FieldType[Sym, Mat[S, OutR x InR]] :: HNil
  , Mat[S, InR x OutC]
  , Mat[S, OutR x OutC]
  , S, Alg
  ] {
    self =>
    def compile[Out[p, a, b]](
      compiler: Alg[Out]
    ): Out[
      FieldType[Sym, Mat[S, OutR x InR]] :: HNil
    , Mat[S, InR x OutC]
    , Mat[S, OutR x OutC]
    ] = compiler.compile(self)
  }

object WeightMat {


  abstract class Diff[
    Sym <: Singleton, S, InR <: XInt, OutR <: XInt, OutC <: XInt
  , Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out]
  ]()
  extends WeightMat[Sym, S, InR, OutR, OutC, Alg] with DiffDag[
    FieldType[Sym, Mat[S, OutR x InR]] :: HNil
  , Mat[S, InR x OutC]
  , Mat[S, OutR x OutC]
  , S, Alg
  ] {
    self =>

    def gradA = new Dag[FieldType[Sym, Mat[S, OutR x InR]] :: HNil, (Mat[S, InR x OutC], Mat[S, OutR x OutC]), Mat[S, InR x OutC], S, Alg] {
        def compile[Out[p, a, b]](
          compiler: Alg[Out]
        ): Out[FieldType[Sym, Mat[S, OutR x InR]] :: HNil, (Mat[S, InR x OutC], Mat[S, OutR x OutC]), Mat[S, InR x OutC]] = compiler.grada(self)
      }

    def gradP = new Dag[FieldType[Sym, Mat[S, OutR x InR]] :: HNil, (Mat[S, InR x OutC], Mat[S, OutR x OutC]), FieldType[Sym, Mat[S, OutR x InR]] :: HNil, S, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[FieldType[Sym, Mat[S, OutR x InR]] :: HNil, (Mat[S, InR x OutC], Mat[S, OutR x OutC]), FieldType[Sym, Mat[S, OutR x InR]] :: HNil] = compiler.gradp(self)
    }

  }

  trait Algebra[S, Alg[out[p, a, b]] <: Algebra[S, Alg, out], Out[p, a, b]]
  extends DagAlgebra[S, Alg, Out] {

    def compile[Sym <: Singleton, InR <: XInt, OutR <: XInt, OutC <: XInt](
      dag: WeightMat[Sym, S, InR, OutR, OutC, Alg]
    ): Out[
      FieldType[Sym, Mat[S, OutR x InR]] :: HNil
    , Mat[S, InR x OutC]
    , Mat[S, OutR x OutC]
    ]

  }


  trait DiffAlgebra[
    S, Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out], Out[p, a, b]
  ] extends Algebra[S, Alg, Out] {

    def grada[
      Sym <: Singleton, InR <: XInt, OutR <: XInt, OutC <: XInt
    ](
      dag: WeightMat.Diff[Sym, S, InR, OutR, OutC, Alg]
    ): Out[FieldType[Sym, Mat[S, OutR x InR]] :: HNil, (Mat[S, InR x OutC], Mat[S, OutR x OutC]), Mat[S, InR x OutC]]

    def gradp[
      Sym <: Singleton, InR <: XInt, OutR <: XInt, OutC <: XInt
    ](
      dag: WeightMat.Diff[Sym, S, InR, OutR, OutC, Alg]
    ): Out[FieldType[Sym, Mat[S, OutR x InR]] :: HNil, (Mat[S, InR x OutC], Mat[S, OutR x OutC]), FieldType[Sym, Mat[S, OutR x InR]] :: HNil]
  }


  trait Dsl[S, Alg[out[p, a, b]] <: nd4j.Algebra[S, Alg, out]] {

    def weightMat[Sym <: Singleton, In <: Dim2[_, _], Out <: Dim2[_, _]](
      implicit same: Dim2SameCol[In, Out]
    ): Dag[
        FieldType[Sym, Mat[S, same.OutR x same.In]] :: HNil
      , Mat[S, same.In x same.OutC]
      , Mat[S, same.OutR x same.OutC]
      , S, Alg
    ] = new WeightMat()

  }  


  trait DiffDsl[
    S, Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out]
  ] {

    def weightMat[Sym <: Singleton, InR <: XInt, OutC <: XInt, OutR <: XInt](
      implicit
        costIn: CostDiffInvertBuilder[Mat[S, InR x OutC], S, Alg]
      , costOut: CostDiffBuilder[Mat[S, OutR x OutC], S, Alg]
      , scalarTimesOut: ScalarTimesBuilder[FieldType[Sym, Mat[S, OutR x InR]] :: HNil, S, Alg]
      , minusA0: MinusBuilder[Mat[S, InR x OutC], S, Alg]
      , minusP0: MinusPBuilder[FieldType[Sym, Mat[S, OutR x InR]] :: HNil, S, Alg]
    ): DiffDag[
        FieldType[Sym, Mat[S, OutR x InR]] :: HNil
      , Mat[S, InR x OutC]
      , Mat[S, OutR x OutC]
      , S, Alg
    ] = new WeightMat.Diff[Sym, S, InR, OutR, OutC, Alg] {
      val costDiffInvert = costIn
      val costDiff = costOut
      val scalarTimes = scalarTimesOut
      val minusA = minusA0
      val minusP = minusP0
    }
  }

}