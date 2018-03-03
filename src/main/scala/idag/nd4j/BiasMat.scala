package neurocat
package idag
package nd4j


import shapeless.{::, HNil}
import typeclasses._
import neurocat.nd4j._
import singleton.ops._
import shapeless.labelled._
import neurocat.nd4j._


class BiasMat[
  Sym <: Singleton, S, R <: XInt, C <: XInt
, Alg[out[p, a, b]] <: BiasMat.Algebra[S, Alg, out]
]() extends Dag[
    FieldType[Sym, Mat[S, R x C]] :: HNil
  , Mat[S, R x C]
  , Mat[S, R x C]
  , S, Alg
  ] {
    self =>
    def compile[Out[p, a, b]](
      compiler: Alg[Out]
    ): Out[
      FieldType[Sym, Mat[S, R x C]] :: HNil
    , Mat[S, R x C]
    , Mat[S, R x C]
    ] = compiler.compile(self)
  }

object BiasMat {

  abstract class Diff[
    Sym <: Singleton, S, R <: XInt : SafeInt, C <: XInt : SafeInt
  , Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out]
  ]()
  extends BiasMat[Sym, S, R, C, Alg] with DiffDag[
    FieldType[Sym, Mat[S, R x C]] :: HNil
  , Mat[S, R x C]
  , Mat[S, R x C]
  , S, Alg
  ] {
    self =>

    def gradA = new Dag[FieldType[Sym, Mat[S, R x C]] :: HNil, (Mat[S, R x C], Mat[S, R x C]), Mat[S, R x C], S, Alg] {
        def compile[Out[p, a, b]](
          compiler: Alg[Out]
        ): Out[FieldType[Sym, Mat[S, R x C]] :: HNil, (Mat[S, R x C], Mat[S, R x C]), Mat[S, R x C]] = compiler.grada(self)
      }

    def gradP = new Dag[FieldType[Sym, Mat[S, R x C]] :: HNil, (Mat[S, R x C], Mat[S, R x C]), FieldType[Sym, Mat[S, R x C]] :: HNil, S, Alg] {
      def compile[Out[p, a, b]](
        compiler: Alg[Out]
      ): Out[FieldType[Sym, Mat[S, R x C]] :: HNil, (Mat[S, R x C], Mat[S, R x C]), FieldType[Sym, Mat[S, R x C]] :: HNil] = compiler.gradp(self)
    }

  }

  trait Algebra[S, Alg[out[p, a, b]] <: Algebra[S, Alg, out], Out[p, a, b]]
  extends DagAlgebra[S, Alg, Out] {

    def compile[Sym <: Singleton, R <: XInt, C <: XInt](
      dag: BiasMat[Sym, S, R, C, Alg]
    ): Out[
      FieldType[Sym, Mat[S, R x C]] :: HNil
    , Mat[S, R x C]
    , Mat[S, R x C]
    ]

  }


  trait DiffAlgebra[
    S, Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out], Out[p, a, b]
  ] extends Algebra[S, Alg, Out] {

    def grada[
      Sym <: Singleton, R <: XInt : SafeInt, C <: XInt : SafeInt
    ](
      dag: BiasMat.Diff[Sym, S, R, C, Alg]
    ): Out[FieldType[Sym, Mat[S, R x C]] :: HNil, (Mat[S, R x C], Mat[S, R x C]), Mat[S, R x C]]

    def gradp[
      Sym <: Singleton, R <: XInt, C <: XInt
    ](
      dag: BiasMat.Diff[Sym, S, R, C, Alg]
    ): Out[FieldType[Sym, Mat[S, R x C]] :: HNil, (Mat[S, R x C], Mat[S, R x C]), FieldType[Sym, Mat[S, R x C]] :: HNil]
  }


  trait Dsl[S, Alg[out[p, a, b]] <: nd4j.Algebra[S, Alg, out]] {

    def bias[Sym <: Singleton, D <: Dim2[_, _]](
      implicit rc: HasRowsCols[D]
    ): Dag[
        FieldType[Sym, Mat[S, rc.Rows x rc.Cols]] :: HNil
      , Mat[S, rc.Rows x rc.Cols]
      , Mat[S, rc.Rows x rc.Cols]
      , S, Alg
    ] = new BiasMat()

  }  


  trait DiffDsl[
    S, Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out]
  ] {

    def bias[Sym <: Singleton, R <: XInt : SafeInt, C <: XInt : SafeInt](
      implicit
        costIn: CostDiffInvertBuilder[Mat[S, R x C], S, Alg]
      , costOut: CostDiffBuilder[Mat[S, R x C], S, Alg]
      , scalarTimesOut: ScalarTimesBuilder[FieldType[Sym, Mat[S, R x C]] :: HNil, S, Alg]
      , minusA0: MinusBuilder[Mat[S, R x C], S, Alg]
      , minusP0: MinusPBuilder[FieldType[Sym, Mat[S, R x C]] :: HNil, S, Alg]
    ): DiffDag[
        FieldType[Sym, Mat[S, R x C]] :: HNil
      , Mat[S, R x C]
      , Mat[S, R x C]
      , S, Alg
    ] = new BiasMat.Diff[Sym, S, R, C, Alg] {
      val costDiffInvert = costIn
      val costDiff = costOut
      val scalarTimes = scalarTimesOut
      val minusA = minusA0
      val minusP = minusP0
    }
  }

}