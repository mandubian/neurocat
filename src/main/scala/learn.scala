package neurocat

import cats.arrow.Category
import cats.Functor
import cats.Show

import shapeless.{HList, HNil, ::}
import spire.implicits._
import spire.math._
import spire.algebra._

import singleton.ops._

/**
  * Learn structure represents a Supervised Learning Algorithm
  * accepting inputs of types A and outputs of type B parametrised by type Params.
  *
  * `Learn` also defines a category of supervised learning algorithms
  *
  * For more details, check this paper https://arxiv.org/pdf/1711.10455.pdf?utm_content=bufferf2efc&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer
  */
trait Learn[A, B] {
  self =>
  type Params

  /** the implement function simply computes an estimated B based on current Params hypothesis
    * and a given value training A
    */
  def implement(params: Params)(a: A): B

  /** the update function updates current Params hypothesis based on training values (A, B)
    *
    * In the case of neural networks, it can be seen as a learning step updating Params(weights) hypothesis
    * by gradient descent with respect to the error computed on current training (A, B) & params hypothesis.
    */
  def update(params: Params)(a: A, b: B): Params

  /** the request function updates current training A based on current Params hypothesis & training (A, B)
    *
    * In the context of several Learns composition, it sends upstream a value can be seen as a learning step backpropagating the gradient to the Input
    * (or upstream in the Learn algorithms composition process)
    */
  def request(params: Params)(a: A, b: B): A

  /** classic category composition refined with Param type */
  def andThen[Q, C](other: Learn.Aux[Q, B, C]): Learn.Aux[(self.Params, Q), A, C] = new Learn[A, C] {
    type Params = (self.Params, Q)

    def implement(params: (self.Params, Q))(a: A) = {
      other.implement(params._2)(self.implement(params._1)(a))
    }

    def update(params: (self.Params, Q))(a: A, c: C) = {
      val b = self.implement(params._1)(a)
      ( self.update(params._1)(a, other.request(params._2)(b, c))
      , other.update(params._2)(b, c)
      )
    }

    def request(params: (self.Params, Q))(a: A, c: C) = {
      self.request(params._1)(a, other.request(params._2)(self.implement(params._1)(a), c))
    }
  }
}


object Learn {
  type Aux[Params0, A, B] = Learn[A, B] { type Params = Params0 }

  /* the Identiy in the supervised learning algorithm category */
  def Id[A] = new Learn[A, A] {
    type Params = Unit

    def implement(params:Params)(a:A) = a

    def update(params:Params)(a1:A, a2:A) = ()

    def request(params:Params)(a1:A, a2:A) = a2
  }

  /* the definition of the category of supervised learning algorithms */
  implicit val symMonoCat = new SymmetricMonoidalCat[Learn] {
    def id[A]: Learn[A, A] = Id

    def compose[A, B, C](f: Learn[B, C], g: Learn[A, B]): Learn[A, C] = new Learn[A, C] {
      type Params = (g.Params, f.Params)

      def implement(params: Params)(a: A) = {
        f.implement(params._2)(g.implement(params._1)(a))
      }

      def update(params: Params)(a: A, c: C) = {
        val b = g.implement(params._1)(a)
        ( g.update(params._1)(a, f.request(params._2)(b, c))
        , f.update(params._2)(b, c)
        )
      }

      def request(params: Params)(a: A, c: C) = {
        g.request(params._1)(a, f.request(params._2)(g.implement(params._1)(a), c))
      }
    }

    def product[A, B, C, D](lhs: Learn[A, B], rhs: Learn[C, D]): Learn[(A, C), (B, D)] = {
      new Learn[(A, C), (B, D)] {
        type Params = (lhs.Params, rhs.Params)

        def implement(params: Params)(ac: (A, C)) = {
          ( lhs.implement(params._1)(ac._1)
          , rhs.implement(params._2)(ac._2)
          )
        }

        def update(params: Params)(ac: (A, C), bd: (B, D)) = {
          ( lhs.update(params._1)(ac._1, bd._1)
          , rhs.update(params._2)(ac._2, bd._2)
          )
        }

        def request(params: Params)(ac: (A, C), bd: (B, D)) = {
          ( lhs.request(params._1)(ac._1, bd._1)
          , rhs.request(params._2)(ac._2, bd._2)
          )
        }
      }
    }

    def braiding[A, B, C, D](l: Learn[(A, C), (B, D)]): Learn[(C, A), (D, B)] = {
      new Learn[(C, A), (D, B)] {
        // ??? shouldn't types in Params be reversed? but not possible because we haven't enough info for it...
        // is it important or not?
        type Params = l.Params

        def implement(params: Params)(ac: (C, A)) = {
          l.implement(params)(ac.swap).swap
        }

        def update(params: Params)(ac: (C, A), bd: (D, B)) = {
          l.update(params)(ac.swap, bd.swap)
        }

        def request(params: Params)(ac: (C, A), bd: (D, B)) = {
          l.request(params)(ac.swap, bd.swap).swap
        }
      }
    }
  }

}

object Neurocat {

  def ParaFn2Learn[M[a, d <: Dim], S, P, In <: Dim, Out <: Dim](
    f: ParametrisedDifferentiable[P, M[S, In], M[S, Out]]
  )(
    eps: S
  , costM: PartialDifferentiable2[M[S, Out], 1]
  , costN: PartialDifferentiableInvertible2[M[S, In], 1]
  )(
    implicit
      mat: MatrixCalculus[M, S]
    , pmod: Module[P, S]
    , mNorm: DimNorm[Out, S]
    , rMultGroup: MultiplicativeGroup[S]
    // not useful... just to display things in debug mode
    , showP: Show[P]
    , showA: Show[M[S, In]]
  ): Learn.Aux[P, M[S, In], M[S, Out]] = new Learn[M[S, In], M[S, Out]] {
      type Params   = P

      def implement(params: P)(a: M[S, In]) = {
        f(params)(a)
      }

      def update(params: P)(a: M[S, In], b: M[S, Out]) = {
        //UI(p, a, b) ≔ p − ε∇pEI(p, a, b)
        val estim: M[S, Out] = f(params)(a)
        val err: M[S, Out] = costM.diff(estim)(b)
        val diffPerP: P = f.diffPerP(params)(a)(err)
        // val paramsUpd = mat.times(mat.mult(diffPerP, err), rMultGroup.times(eps, mNorm.norm))
        val paramsUpd: P = pmod.timesr(diffPerP, rMultGroup.times(eps, mNorm.norm))
        // println(s"mult:${rMultGroup.times(eps, mNorm.norm)} paramsUpd:${showP.show(paramsUpd)}")
        pmod.minus(params, paramsUpd)
      }

      def request(params: P)(a: M[S, In], b: M[S, Out]) = {
        // rI(p, a, b) ≔ fa((1/α(m))*∇aEI(p, a, b))
        val estim: M[S, Out] = f(params)(a)
        val err: M[S, Out] = costM.diff(estim)(b)
        val diffPerA: M[S, In] = f.diffPerA(params)(a)(err)
        val aUpdate = mat.times(diffPerA, rMultGroup.reciprocal(mNorm.norm))
        // println(s"aUpdate:${showA.show(aUpdate)}")
        costN.diffInvert(a)(aUpdate)
      }
  }

  def NNetLayer2ParaFn[M[a, d <: Dim], S, InR <: XInt, OutR <: XInt, OutC <: XInt](
    layer: NNetLayer[M, S, InR, OutR, OutC]
  )(implicit
    mat: MatrixCalculus[M, S]
  ): EuclideanParametrisedFunction[M, S, OutR x InR, InR x OutC, OutR x OutC] =
    new EuclideanParametrisedFunction[M, S, OutR x InR, InR x OutC, OutR x OutC] {
      def apply(params: M[S, OutR x InR])(a: M[S, InR x OutC]): M[S, OutR x OutC] = {
        layer.activation(layer.output(params)(a))
      }

      def diffPerP(params: M[S, OutR x InR])(a: M[S, InR x OutC])(e: M[S, OutR x OutC]): M[S, OutR x InR] = {
        val s: M[S, OutR x OutC] = layer.activation.diff(layer.output(params)(a))
        val t: M[S, OutR x InR] = layer.output.diffPerP(params)(a)(mat.hadamard(e, s))
        t
      }

      def diffPerA(params: M[S, OutR x InR])(a: M[S, InR x OutC])(e: M[S, OutR x OutC]): M[S, InR x OutC] = {
        val s: M[S, OutR x OutC] = layer.activation.diff(layer.output(params)(a))
        layer.output.diffPerA(params)(a)(mat.hadamard(e, s))
      }

    }


}


