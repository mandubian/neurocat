import singleton.ops._

package object neurocat {
  // a simple type alias for Dim2
  type x[Rows <: XInt, Cols <: XInt] = Dim2[Rows, Cols]

  type SafeRows[R <: XInt] = impl.OpInt[RequireMsg[R > 0, "Can't create a matrix with 0 (or less) rows"] ==> SafeInt[R]]
  type SafeCols[C <: XInt] = impl.OpInt[RequireMsg[C > 0, "Can't create a matrix with 0 (or less) columns"] ==> SafeInt[C]]

}

