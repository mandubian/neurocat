package neurocat

import singleton.ops._


trait ParametrisedFunction[A, B] {
  type Params

  def apply(params: Params)(in: A): B
}


object ParametrisedFunction {
  def Id[A] = new ParametrisedFunction[A, A] {
    type Params = Unit

    def apply(params: Params)(in: A): A = in
  }

  implicit val symMonoCat = new SymmetricMonoidalCat[ParametrisedFunction] {
    def id[A]: ParametrisedFunction[A, A] = Id

    def compose[A, B, C](f: ParametrisedFunction[B, C], g: ParametrisedFunction[A, B]): ParametrisedFunction[A, C] = new ParametrisedFunction[A, C] {
      type Params = (g.Params, f.Params)

      def apply(params: Params)(a: A): C = f(params._2)(g(params._1)(a))
    }

    def product[A, B, C, D](lhs: ParametrisedFunction[A, B], rhs: ParametrisedFunction[C, D]): ParametrisedFunction[(A, C), (B, D)] = new ParametrisedFunction[(A, C), (B, D)] {
      type Params = (lhs.Params, rhs.Params)

      def apply(params: Params)(a: (A, C)): (B, D) = {
        (lhs(params._1)(a._1), rhs(params._2)(a._2))
      }
    }

    def braiding[A, B, C, D](l: ParametrisedFunction[(A, C), (B, D)]): ParametrisedFunction[(C, A), (D, B)] = {
      new ParametrisedFunction[(C, A), (D, B)] {
        // ??? shouldn't types in Params be reversed? but not possible because we haven't enough info for it...
        // is it important or not?
        type Params = l.Params

        def apply(params: Params)(a: (C, A)): (D, B) = {
          l(params)(a.swap).swap
        }
      }
    }
  }
}


trait Differentiable[A, B] extends (A => B) {
  def diff(in: A): B
}

trait PartialDifferentiable2[A, Idx <: XInt] extends (A => A => A) {
  def diff: A => A => A
}

trait PartialDifferentiableInvertible2[A, Idx <: XInt] extends PartialDifferentiable2[A, Idx] {
  def diffInvert: A => A => A
}

trait ParametrisedDifferentiable[P, A, B] extends ParametrisedFunction[A, B] {
  type Params = P
  def diffPerP(p: P)(a: A)(b: B): P
  def diffPerA(p: P)(a: A)(b: B): A
}

/** a euclidean differentiable parametrised function */
trait EuclideanParametrisedFunction[Arr[a, d <: Dim], R, P <: Dim, N <: Dim, M <: Dim]
  extends ParametrisedDifferentiable[Arr[R, P], Arr[R, N], Arr[R, M]]
