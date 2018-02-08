package neurocat
package idag

import shapeless.{HList, HNil, ::}
import cats.Monoid


trait Comonoid[M] {
  def split(m: M): (M, M)
  def destroy(m: M): Unit
}

object Comonoid {
  def apply[M](implicit M: Comonoid[M]): Comonoid[M] = M
}

trait Diff[A, B] extends (A => B) {
  type GradA
  def diff(in: A): GradA
}

trait Parametrised[P, A, B] extends Function2[P, A, B]

trait ParametrisedDiff[P, A, B] extends Parametrised[P, A, B] {
  // gradient/jacobian per P 
  type GradP
  // gradient/jacobian per A
  type GradA
  def gradP(p: P, a: A): GradP
  def gradA(p: P, a: A): GradA
}

sealed trait IDag[A, B] extends Diff[A, B]

case class Edge[A, B](f: Diff[A, B]) extends IDag[A, B] {

  def apply(a: A): B = f(a)

  type GradA = f.GradA

  def diff(a: A): GradA = f.diff(a)
}

final case class Init[P : Monoid](f: () => P) extends IDag[Unit, P] {
  def apply(u:Unit): P = f()

  type GradA = P

  def diff(a: Unit): P = Monoid[P].empty
}

final case class Discard[A](f: A => Unit) extends IDag[A, Unit] {

  def apply(a:A): Unit = ()

  type GradA = Unit

  def diff(a: A): Unit = Unit
}


final case class ParametrisedDag[P, A, B](p: Init[P], f: ParametrisedDiff[P, A, B]) extends IDag[A, B] {

  def apply(a: A): B = f(p(), a)

  type GradA = f.GradA

  def diff(a: A): GradA = f.gradA(p(), a)
}

final case class Parallel[A, B, C, D](
  f: IDag[A, C], g: IDag[B, D]
) extends IDag[(A, B), (C, D)] {

  def apply(ab: (A, B)): (C, D) = {
    (f(ab._1), g(ab._2))
  }
  
  type GradA = (f.GradA, g.GradA)
  def diff(ab: (A, B)): (f.GradA, g.GradA) = (f.diff(ab._1), g.diff(ab._2))
}

final case class Compose[A, B, C](
  g: IDag[B, C], f: IDag[A, B]
) extends IDag[A, C] {

  def apply(a: A): C = g(f(a))

  type GradA = (f.GradA,  g.GradA)
  
  def diff(a: A): (f.GradA, g.GradA) = {
    val gfa = f.diff(a)
    (gfa, g.diff(f(a)))
  }
}

// monoid should be like multiplicative monoid
final case class Split[A : Comonoid : Monoid]() extends IDag[A, (A, A)] {
  def apply(a: A): (A, A) = Comonoid[A].split(a)

  type GradA = (A, A)
  
  def diff(a: A): (A, A) = (Monoid[A].empty, Monoid[A].empty)
}


// monoid should be like multiplicative monoid
final case class Join[A : Monoid]() extends IDag[(A, A), A] {
  def apply(a: (A, A)): A = Monoid[A].combine(a._1, a._2)

  type GradA = A
  
  def diff(a: (A, A)): A = Monoid[A].empty
}


object IDag {

  /*
   *  A -----> C       A ---  ---> D
   *              ==>       \/
   *              ==>       /\
   *  B -----> D       B ---  ---> C
   */
  def braid[A, B, C, D](p: Parallel[A, B, C, D]) = new IDag[(A, B), (D, C)] {

    def apply(ab: (A, B)): (D, C) = {
      p(ab).swap
    }
    
    type GradA = (p.g.GradA, p.f.GradA)
    def diff(ab: (A, B)): (p.g.GradA, p.f.GradA) = p.diff(ab).swap
  }

  def rightAssociate[A, B, C, D](f: IDag[((A, B), C), D]) = new IDag[(A, (B, C)), D] {

    def apply(abc: (A, (B, C))): D = {
      val (a, (b, c)) = abc
      f(((a, b), c))
    }
    
    type GradA = f.GradA
    
    def diff(abc: (A, (B, C))): f.GradA = {
      val (a, (b, c)) = abc
      f.diff(((a, b), c))
    }
  }

  def leftAssociate[A, B, C, D](f: IDag[(A, (B, C)), D]) = new IDag[((A, B), C), D] {

    def apply(abc: ((A, B), C)): D = {
      val ((a, b), c) = abc
      f((a, (b, c)))
    }
    
    type GradA = f.GradA
    
    def diff(abc: ((A, B), C)): f.GradA = {
      val ((a, b), c) = abc
      f.diff((a, (b, c)))
    }
  }

  def init[P : Monoid](p: => P) = Init(() => p)

  // case class NNetLayer[M[a, d <: Dim], R, W <: Dim2[_, _], In <: Dim2[_, _], Out <: Dim2[_, _]](
  //   val activation: Differentiable[M[R, Out], M[R, Out]]
  // , val body: ParametrisedDifferentiable[M[R, W], M[R, In], M[R, Out]]
  // )

  def nnetLayer[M[a, d <: Dim], S, W <: Dim2[_, _], In <: Dim2[_, _], Out <: Dim2[_, _]](
    initWeights: M[S, W]
  , activation: Diff[M[S, Out], M[S, Out]]
  , body: ParametrisedDiff[M[S, W], M[S, In], M[S, Out]]
  )(implicit m: Monoid[M[S, W]]): IDag[M[S, In], M[S, Out]] = {
    Compose(
      Edge(activation)
    , ParametrisedDag(init(initWeights), body)
    )
  }


  // def idag2Learn(
  //   idag: IDag[A, B]
  // ): Learn.Aux[P, A, B] = {
  //   idag match {
  //     case Init(p) =>
  //       new Learn[A, B] {
  //         type Params   = B

  //         def implement(params: B)(a: A) = {
  //           p()
  //         }
  //       }
  //   }
  // }

}