package neurocat

import singleton.ops._

trait ShowType[T] {
  def showType: String
}

/** a very basic dimension */
sealed trait Dim

/* 1-Dimension for vectors */
trait Dim1[I <: XInt] extends Dim

object Dim1 {
  implicit def showType[I <: XInt](implicit c: SafeInt[I]) = new ShowType[Dim1[I]] {
    def showType = s"""[${c.value}]"""
  }
}

/* 2-Dimension... by convention, first type param is number of rows, second is number of columns */
trait Dim2[Rows <: XInt, Cols <: XInt] extends Dim

object Dim2 {
  implicit def showType[I <: XInt, J <: XInt](implicit ci: SafeInt[I], cj: SafeInt[J]) = new ShowType[Dim2[I, J]] {
    def showType = s"""[${ci.value}, ${cj.value}]"""
  }
}

trait DimMult[D1 <: Dim, D2 <: Dim] {
  type R <: Dim
}


object DimMult {
  type Aux[D1 <: Dim, D2 <: Dim, R0 <: Dim] = DimMult[D1, D2] { type R = R0 }

  implicit def d2[A <: XInt, B <: XInt, C <: XInt] =
    new DimMult[Dim2[A, B], Dim2[B, C]] {
      type R = Dim2[A, C]
    }
}

trait Mult[A, B] {
  type R

  def mult(a: A, b: B): R
}

object Mult {
  type Aux[A, B, R0] = Mult[A, B] { type R = R0 }
}

trait HasRowsCols[D <: Dim] {
  type Rows <: XInt
  type Cols <: XInt

  implicit def rr: Require[Rows > 0]
  implicit def rs: SafeInt[Rows]
  implicit def cr: Require[Cols > 0]
  implicit def cs: SafeInt[Cols]
}

object HasRowsCols {
  implicit def dim2[I <: XInt, J <: XInt](
    implicit rRows: Require[I > 0], rCols: Require[J > 0], opRows: SafeInt[I], opCols: SafeInt[J]
  ) = new HasRowsCols[Dim2[I, J]] {
    type Rows = I
    type Cols = J

    implicit def rr = rRows
    implicit def rs = opRows
    implicit def cr = rCols
    implicit def cs = opCols
  }
}


trait RowsCols[D] {
  def rows: Int
  def cols: Int
}

object RowsCols {
  implicit def d2[A <: XInt, B <: XInt](
    implicit opA: SafeInt[A], opB: SafeInt[B]
  ) = new RowsCols[A x B] {
    def rows = opA.value
    def cols = opB.value
  }
}

trait Dim2SameCol[D1 <: Dim2[_, _], D2 <: Dim2[_, _]] {
  type In <: XInt
  type OutR <: XInt
  type OutC <: XInt
}

object Dim2SameCol {
  implicit def trivial[A <: XInt, B <: XInt, C <: XInt] = new Dim2SameCol[Dim2[A, C], Dim2[B, C]] {
    type In = A
    type OutR = B
    type OutC = C
  }
}

trait DimNorm[M <: Dim, R] {
  def norm: R
}

object DimNorm {
  implicit def d2[I <: XInt, J <: XInt](
    implicit opRows: SafeInt[I], opCols: SafeInt[J]
  ) = new DimNorm[Dim2[I, J], Double] {
    def norm = (opRows.value * opCols.value).toDouble
  }
}