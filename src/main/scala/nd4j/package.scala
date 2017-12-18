package neurocat

import org.nd4j.linalg.activations.Activation
import org.nd4j.linalg.factory.Nd4j
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.NDArrayFactory
import scala.collection.breakOut

import cats.Show
import shapeless.{HNil, ::}
import spire.algebra.{Module, Rng}

import singleton.ops._


package object nd4j {

  sealed trait NDOrdering {
    val value:Char
  }
  object NDOrdering{
    case object Fortran extends NDOrdering{
      override val value: Char = NDArrayFactory.FORTRAN
    }
    case object C extends NDOrdering{
      override val value: Char = NDArrayFactory.C
    }
    def apply(char:Char): NDOrdering = char.toLower match{
      case 'c' => C
      case 'f' => Fortran
      case _ => throw new IllegalArgumentException("NDimensional Ordering accepts only 'c' or 'f'.")
    }
  }


  implicit def moduleHNil[S, R <: XInt, C <: XInt](
    implicit
      n: Numeric[S]
    , sRng: Rng[S]
    , opRows: SafeInt[R], opCols: SafeInt[C]
  ): Module[Mat[S, R x C] :: HNil, S] = new Module[Mat[S, R x C] :: HNil, S] {
    def negate(a: Mat[S, R x C] :: HNil): Mat[S, R x C] :: HNil =
      new Mat[S, R x C](a.head.value.negi()) :: HNil

    def zero: Mat[S, R x C] :: HNil =
      new Mat[S, R x C](Nd4j.zeros(Array(opRows.value, opCols.value), Nd4j.order())) :: HNil

    def plus(x: Mat[S, R x C] :: HNil, y: Mat[S, R x C] :: HNil): Mat[S, R x C] :: HNil =
      new Mat[S, R x C](x.head.value.add(y.head.value)) :: HNil

    def timesl(r: S, v: Mat[S, R x C] :: HNil): Mat[S, R x C] :: HNil =
      new Mat[S, R x C](v.head.value.muli(n.toDouble(r))) :: HNil

    implicit def scalar = sRng
  }


  implicit def show[R, D <: Dim](implicit st: ShowType[D]) = new Show[Mat[R, D] :: HNil] {
    def show(a: Mat[R, D] :: HNil): String =
      s"""Mat${st.showType}(${a.head.value})"""
  }

  /** copied from ND4S... not useful for now */
  // implicit def intColl2INDArray(s:Seq[Int]):IntArray2INDArray = new IntArray2INDArray(s.toArray)
  // implicit def intArray2INDArray(s:Array[Int]):IntArray2INDArray = new IntArray2INDArray(s)
  // implicit def jintColl2INDArray(s:Seq[java.lang.Integer]):IntArray2INDArray = new IntArray2INDArray(s.map(x => x: Int)(breakOut))

  // class IntArray2INDArray(val underlying: Array[Int]) extends AnyVal {
  //   def mkNDArray(shape: Array[Int], ord: NDOrdering = NDOrdering(Nd4j.order()), offset: Int = 0): INDArray = Nd4j.create(underlying.map(_.toFloat), shape, ord.value, offset)

  //   def asNDArray(shape: Int*): INDArray = Nd4j.create(underlying.map(_.toFloat), shape.toArray)

  //   def toNDArray: INDArray = Nd4j.create(underlying.map(_.toFloat).toArray)
  // }

  // implicit class IntMtrix2INDArray(val underlying: Seq[Seq[Int]]) extends AnyVal {
  //   def mkNDArray(ord: NDOrdering): INDArray = Nd4j.create(underlying.map(_.map(_.toFloat).toArray).toArray, ord.value)
  //   def toNDArray: INDArray = Nd4j.create(underlying.map(_.map(_.toFloat).toArray).toArray)
  // }

  // implicit class IntArrayMtrix2INDArray(val underlying: Array[Array[Int]]) extends AnyVal {
  //   def mkNDArray(ord: NDOrdering): INDArray = Nd4j.create(underlying.map(_.map(_.toFloat)), ord.value)
  //   def toNDArray: INDArray = Nd4j.create(underlying.map(_.map(_.toFloat)))
  // }

}