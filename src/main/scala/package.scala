import singleton.ops._

package object neurocat {
  // a simple type alias for Dim2
  type x[Rows <: XInt, Cols <: XInt] = Dim2[Rows, Cols]
}