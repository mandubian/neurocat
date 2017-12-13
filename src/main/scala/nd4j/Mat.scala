package neurocat
package nd4j

import singleton.ops._

import org.nd4j.linalg.factory.Nd4j
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.NDArrayFactory

import cats.Show
import spire.algebra.{Module, Rng}

/**
  * A compile-time sized array relying on ND4J INDArray
  */
class Mat[R, D <: Dim] private[neurocat] (val value: INDArray)

object Mat {

  def fromINDArray2[R, D <: Dim2[_, _]](value: INDArray)(
    implicit hrc: HasRowsCols[D]
  ): Mat[R, D] = new Mat[R, D](
    value.reshape(hrc.rs.value, hrc.cs.value)
  )

  def columnVector[R, Rows <: XInt](array: Array[R])(
    implicit num: Numeric[R], rRows: Require[Rows > 0], opRows: SafeInt[Rows]
  ): Mat[R, Rows x 1] = {
    require(array.length == opRows.value, s"Array is ${array.length} long but should be ${opRows.value} long")
    val size = Array(opRows.value, 1)
    new Mat[R, Rows x 1](
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

  implicit def matrixCalculs[S](implicit n: Numeric[S]): MatrixCalculus[Mat, S] = new MatrixCalculus[Mat, S] {
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

  implicit def rowTraversable[S, R <: XInt, C <: XInt](
    implicit rowsNb: SafeInt[R]
  ) = new RowTraversable[Mat[S, R x C], Mat[S, C x 1]] {
    def foreachRow[U](t: Mat[S, R x C]) (f: Mat[S, C x 1] => U): Unit = {
      (0 to rowsNb.value).foreach { i =>
        f(new Mat[S, C x 1](t.value.getRow(i).transposei()))
      }
    }
  }

  // hard coded for Mat for now
  def train[P, S, InRows <: XInt, OutRows <: XInt, NbSamples <: XInt](
    learn: Learn.Aux[P, Mat[S, InRows x 1], Mat[S, OutRows x 1]]
  )(
    initParams: P
  , trainDataIn: Mat[S, NbSamples x InRows]
  , trainDataOut: Mat[S, NbSamples x OutRows]
  )(
    implicit nbSamples: SafeInt[NbSamples]
    , showInRow: Show[Mat[S, InRows x 1]]
    , showOutRows: Show[Mat[S, OutRows x 1]]
    , showP: Show[P]
  ): P = {
    var params: P = initParams
    println(s"params0:${showP.show(params)}")
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

