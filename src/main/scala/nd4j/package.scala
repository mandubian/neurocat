package neurocat

import org.nd4j.linalg.activations.Activation
import org.nd4j.linalg.factory.Nd4j
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.NDArrayFactory
import scala.collection.breakOut

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