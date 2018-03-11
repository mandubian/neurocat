package neurocat

import singleton.ops._


/* A very basic Matrix Calculus defining ops on dimensional Mays relying on singleton-types
 *
 * We would need here a Spire extension with dimensions based on singleton-types defining
 * corresponding structures like commutative/additive/multiplicative monoids/(semi)Ri(n)gs etc...
 * 
 * M[s, d <: Dim] is a matrix of dimension d & containing scalars s
 */
trait MatrixCalculus[M[s, d <: Dim], Scalar] {
  def mult[D1 <: Dim, D2 <: Dim](s1: M[Scalar, D1], s: M[Scalar, D2])(implicit mult: DimMult[D1, D2]): M[Scalar, mult.R]

  def hadamard[D1 <: Dim](s1: M[Scalar, D1], s: M[Scalar, D1]): M[Scalar, D1]

  def times[D1 <: Dim](s1: M[Scalar, D1], s: Scalar): M[Scalar, D1]

  def minus[D1 <: Dim](s1: M[Scalar, D1], s2: M[Scalar, D1]): M[Scalar, D1]

  def transpose[D1 <: Dim](s1: M[Scalar, D1])(implicit transp: Transposable[D1]): M[Scalar, transp.R]

  // def zeros[D1 <: Dim]: M[Scalar, D1]

  // def ones[D1 <: Dim]: M[Scalar, D1]
}


trait Transposable[D <: Dim] {
  type R <: Dim
}

object Transposable {
  implicit def d2[Rows <: XInt, Cols <: XInt] = new Transposable[Rows x Cols] {
    type R = Cols x Rows
  }
}

