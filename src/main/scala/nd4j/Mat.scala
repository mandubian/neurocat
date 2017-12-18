package neurocat
package nd4j

import singleton.ops._

import org.nd4j.linalg.factory.Nd4j
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.NDArrayFactory

import cats.Show
import spire.algebra.{Module, Rng}
import shapeless.ops.hlist.IsHCons

import shapeless.{HNil, ::}

/**
  * A compile-time sized array relying on ND4J INDArray
  */
class Mat[S, D <: Dim] private[neurocat] (val value: INDArray) {
  def transpose(implicit transp: Transposable[D]): Mat[S, transp.R] = {
    new Mat[S, transp.R](value.transpose())
  }
}

object Mat {

  def fromINDArray2[R, D <: Dim2[_, _]](value: INDArray)(
    implicit hrc: HasRowsCols[D]
  ): Mat[R, D] = new Mat[R, D](
    value.reshape(hrc.rs.value, hrc.cs.value)
  )


  /** build a column vector from an array with (R > 0) rows
    * If array hasn't exactly R elements, it will crash at runtime
    */
  def columnVector[S, Rows <: XInt](array: Array[S])(
    implicit
      num: Numeric[S], opRows: SafeRows[Rows]
  ): Mat[S, Rows x 1] = {
    require(array.length == opRows.value, s"For ColumnVector, Array is ${array.length} long but should be ${opRows.value} long")
    val size = Array(opRows.value, 1)
    new Mat[S, Rows x 1](
      Nd4j.create(array.map(x => num.toDouble(x)), size)
    )
  }

  /** build a row vector from an array with (R > 0) rows
    * If array hasn't exactly R elements, it will crash at runtime
    */
  def rowVector[S, Cols <: XInt](array: Array[S])(
    implicit
      num: Numeric[S], opCols: SafeCols[Cols]
  ): Mat[S, 1 x Cols] = {
    require(array.length == opCols.value, s"For RowVector, Array is ${array.length} long but should be ${opCols.value} long")
    val size = Array(1, opCols.value)
    new Mat[S, 1 x Cols](
      Nd4j.create(array.map(x => num.toDouble(x)), size)
    )
  }

  def fromArrays[R, D <: Dim2[_, _]](arrays: Array[Array[R]])(
    implicit num: Numeric[R], rcs: RowsCols[D]
  ): Mat[R, D] = {
    require(arrays.length == rcs.rows && arrays.forall(_.length == rcs.cols))
    new Mat[R, D](
      Nd4j.create(arrays.map(_.map(x => num.toDouble(x))))
    )
  } 

  def randomD2[R, D <: Dim2[_, _]](min: Double, max: Double)(
    implicit rcs: RowsCols[D]
  ): Mat[R, D] = {
    new Mat[R, D](
      Nd4j.rand(Array(rcs.rows, rcs.cols), min, max, Nd4j.getRandom())
    )
  }

  implicit def show[R, D <: Dim](implicit st: ShowType[D]) = new Show[Mat[R, D]] {
    def show(a: Mat[R, D]): String =
      s"""Mat${st.showType}(${a.value})"""
  }

  implicit def matrixCalculus[S](implicit n: Numeric[S]): MatrixCalculus[Mat, S] = new MatrixCalculus[Mat, S] {
    def mult[D1 <: Dim, D2 <: Dim](a1: Mat[S, D1], a2: Mat[S, D2])(implicit mult: DimMult[D1, D2]): Mat[S, mult.R] = {
      new Mat[S, mult.R](a1.value.mmul(a2.value))
    }

    def hadamard[D1 <: Dim](a1: Mat[S, D1], a2: Mat[S, D1]): Mat[S, D1] = {
      new Mat[S, D1](a1.value.mul(a2.value))
    }

    def times[D1 <: Dim](a: Mat[S, D1], s: S): Mat[S, D1] = {
      new Mat[S, D1](a.value.muli(n.toDouble(s)))
    }

    def minus[D1 <: Dim](a1: Mat[S, D1], a2: Mat[S, D1]): Mat[S, D1] = {
      new Mat[S, D1](a1.value.rsubi(a2.value))
    }

    def transpose[D1 <: Dim](a: Mat[S, D1])(implicit transp: Transposable[D1]): Mat[S, transp.R] = {
      new Mat[S, transp.R](a.value.transpose())
    }
  }

  implicit def module[S, R <: XInt, C <: XInt](
    implicit
      n: Numeric[S]
    , sRng: Rng[S]
    , opRows: SafeInt[R], opCols: SafeInt[C]
  ): Module[Mat[S, R x C], S] = new Module[Mat[S, R x C], S] {
    def negate(a: Mat[S, R x C]): Mat[S, R x C] =
      new Mat[S, R x C](a.value.negi())

    def zero: Mat[S, R x C] =
      new Mat[S, R x C](Nd4j.zeros(Array(opRows.value, opCols.value), Nd4j.order()))

    def plus(x: Mat[S, R x C], y: Mat[S, R x C]): Mat[S, R x C] =
      new Mat[S, R x C](x.value.add(y.value))

    def timesl(r: S, v: Mat[S, R x C]): Mat[S, R x C] =
      new Mat[S, R x C](v.value.muli(n.toDouble(r)))

    implicit def scalar = sRng
  }

  // implicit def rowTraversable[S, R <: XInt, C <: XInt](
  //   implicit rowsNb: SafeInt[R]
  // ) = new RowTraversable[Mat[S, R x C], Mat[S, C x 1]] {
  //   def foreachRow[U](t: Mat[S, R x C]) (f: Mat[S, C x 1] => U): Unit = {
  //     (0 to rowsNb.value).foreach { i =>
  //       f(new Mat[S, C x 1](t.value.getRow(i).transposei()))
  //     }
  //   }
  // }

  // hard coded for Mat for now
  def train[P, S, InRows <: XInt, OutRows <: XInt, NbSamples <: XInt](
    learn: Learn.Aux[P, Mat[S, InRows x 1], Mat[S, OutRows x 1]]
  )(
    initParams: P
  , trainDataIn: Mat[S, NbSamples x InRows]
  , trainDataOut: Mat[S, NbSamples x OutRows]
  )(
    implicit nbSamples: SafeInt[NbSamples]
    // , showInRow: Show[Mat[S, InRows x 1]]
    // , showOutRows: Show[Mat[S, OutRows x 1]]
    // , showP: Show[P]
  ): P = {
    var params: P = initParams
    // println(s"params0:${showP.show(params)}")
    (0 until 10).foreach { _ =>
      (0 until nbSamples).foreach { i =>
        val inRow = new Mat[S, InRows x 1](trainDataIn.value.getRow(i).transpose())
        val outRow = new Mat[S, OutRows x 1](trainDataOut.value.getRow(i).transpose())
        // println(s"params:${showP.show(params)} inRow:${showInRow.show(inRow)} outRow: ${showOutRows.show(outRow)}")
        params = learn.update(params)(inRow, outRow)
      }
    }

    params
  }  
}

sealed abstract class DataSet[Row, NbSamples <: XInt](
  implicit nbSamples: SafeInt[NbSamples]
) {
  private[nd4j] def getRow(i: Int): Row

  def foreachRow[U](f: Row => U): Unit = {
    (0 until nbSamples).foreach { i =>
      f(getRow(i))
    }
  }

  def zip[Row2](other: DataSet[Row2, NbSamples]): DataSet[(Row, Row2), NbSamples] = {
    ProductDataSet(this, other)
  }
} 

case class ProductDataSet[Row1, Row2, NbSamples <: XInt] private(
  d1: DataSet[Row1, NbSamples], d2: DataSet[Row2, NbSamples]
)(
  implicit nbSamples: SafeInt[NbSamples]
) extends DataSet[(Row1, Row2), NbSamples] {

  private[nd4j] def getRow(i: Int): (Row1, Row2) = {
    (d1.getRow(i), d2.getRow(i))
  }

} 

case class SingleDataSet[S, D <: Dim, NbSamples <: XInt : SafeInt] private(data: INDArray) extends DataSet[Mat[S, D], NbSamples] {

  private[nd4j] def getRow(i: Int): Mat[S, D] = {
    new Mat[S, D](data.getRow(i).transpose())
  }

  def getRow[I <: XInt](
    implicit i: Require[I < NbSamples] ==> SafeInt[I]
  ): Mat[S, D] = {
    getRow(i)
  }

}

object DataSet {
  implicit val rowTraversable: RowTraversable[DataSet] = new RowTraversable[DataSet] {
    def foreachRow[Row, NbSamples <: XInt, U](a: DataSet[Row, NbSamples])(f: Row => U): Unit =
      a.foreachRow(f)
  }

  trait DataSetBuilder[NbSamples <: XInt] {
    def apply[S, Cols <: XInt](
      data: Mat[S, NbSamples x Cols]
    )(implicit nbSamples: SafeInt[NbSamples]): DataSet[Mat[S, Cols x 1], NbSamples]
  }

  def apply[NbSamples <: XInt] = new DataSetBuilder[NbSamples] {
    def apply[S, Cols <: XInt](
      data: Mat[S, NbSamples x Cols]
    )(implicit nbSamples: SafeInt[NbSamples]): DataSet[Mat[S, Cols x 1], NbSamples] =
      SingleDataSet[S, Cols x 1, NbSamples](data.value)
  }

  // abstract class DataSetBuilder2[Rows <: XInt, NbSamples <: XInt : SafeInt] {
  //   def apply[S](
  //     data: Mat[S, NbSamples x Rows]
  //   ): DataSet[Mat[S, Rows x 1], NbSamples]
  // }


  // def apply[RowDim <: Dim2[_, 1], NbSamples <: XInt : SafeInt](
  //   implicit rcs: RowsCols[RowDim]
  // ) = new DataSetBuilder2[rcs.Rows, NbSamples] {
  //   def apply[S](
  //     data: Mat[S, NbSamples x rcs.Rows]
  //   ): DataSet[Mat[S, rcs.Rows x 1], NbSamples] =
  //     SingleDataSet[S, rcs.Rows x 1, NbSamples](data.value)
  // }
}