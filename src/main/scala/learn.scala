package neurocat

import cats.arrow.Category
import cats.Functor
import cats.Show

import shapeless.{HList, HNil, ::, Nat}
import shapeless.ops.hlist.{Prepend, Take, Drop, Length}
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

}

trait HLearn[P <: HList, A, B] extends Learn[A, B] {
  self =>


  type Params = P

  /** classic category composition refined with Param type */
  def andThen[Q <: HList, C, All <: HList, PL <: Nat](other: HLearn[Q, B, C])(
    implicit
      prepend : Prepend.Aux[P, Q, All]
    , pLength : Length.Aux[P, PL]
    , take: Take.Aux[All, PL, P]
    , drop: Drop.Aux[All, PL, Q]
  ): HLearn[All, A, C] = new HLearn[All, A, C] {

    def implement(params: Params)(a: A) = {
      other.implement(drop(params))(self.implement(take(params))(a))
    }

    def update(params: Params)(a: A, c: C) = {
      val b = self.implement(take(params))(a)
      prepend(
        self.update(take(params))(a, other.request(drop(params))(b, c))
      , other.update(drop(params))(b, c)
      )
    }

    def request(params: Params)(a: A, c: C) = {
      self.request(take(params))(a, other.request(drop(params))(self.implement(take(params))(a), c))
    }
  }
}


object Learn {
  type Aux[Params0, A, B] = Learn[A, B] { type Params = Params0 }

  /* the Identiy in the supervised learning algorithm category */
  def Id[A] = new Learn[A, A] {
    type Params = HNil

    def implement(params:Params)(a:A) = a

    def update(params:Params)(a1:A, a2:A) = HNil

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


  object Trainer {

    def naive[
      DataSet[row, nb <: XInt]
    ](implicit rowTr: RowTraversable[DataSet]): Trainer[DataSet] =
      new Trainer[DataSet] {      
        def train[Params <: HList, In, Out, NbSamples <: XInt](
          learn: HLearn[Params, In, Out]
        )(
          initParams: Params
        , trainingData: DataSet[(In, Out), NbSamples]
        ): Params = {
          var params = initParams

          rowTr.foreachRow(trainingData) {
            case (inRow, outRow) =>
              params = learn.update(params)(inRow, outRow)
          }

          params
        }
      }

  }
}

trait NeurocatFunctors {

  def ParaFn2Learn0[M[a, d <: Dim], S, P, In <: Dim, Out <: Dim](
    f: ParametrisedDifferentiable[P, M[S, In], M[S, Out]]
  )(
    eps: S
  , cost: MatrixFunction2[M, S]
  )(
    implicit
      mat: MatrixCalculus[M, S]
    , pmod: Module[P, S]
    , mNorm: DimNorm[Out, S]
    , rMultGroup: MultiplicativeGroup[S]
  ): Learn.Aux[P, M[S, In], M[S, Out]] = new Learn[M[S, In], M[S, Out]] {
      type Params   = P

      def implement(params: P)(a: M[S, In]) = {
        f(params)(a)
      }

      def update(params: P)(a: M[S, In], b: M[S, Out]) = {
        //UI(p, a, b) ≔ p − ε∇pEI(p, a, b)
        val estim: M[S, Out] = f(params)(a)
        val err: M[S, Out] = cost[Out].diff(estim)(b)
        val diffPerP: P = f.diffPerP(params)(a)(err)
        // val paramsUpd = mat.times(mat.mult(diffPerP, err), rMultGroup.times(eps, mNorm.norm))
        val paramsUpd: P = pmod.timesr(diffPerP, rMultGroup.times(eps, mNorm.norm))
        // println(s"mult:${rMultGroup.times(eps, mNorm.norm)} paramsUpd:${showP.show(paramsUpd)}")
        pmod.minus(params, paramsUpd)
      }

      def request(params: P)(a: M[S, In], b: M[S, Out]) = {
        // rI(p, a, b) ≔ fa((1/α(m))*∇aEI(p, a, b))
        val estim: M[S, Out] = f(params)(a)
        val err: M[S, Out] = cost[Out].diff(estim)(b)
        val diffPerA: M[S, In] = f.diffPerA(params)(a)(err)
        val aUpdate = mat.times(diffPerA, rMultGroup.reciprocal(mNorm.norm))
        // println(s"aUpdate:${showA.show(aUpdate)}")
        cost[In].diffInvert(a)(aUpdate)
      }
  }

  def ParaFn2Learn[M[a, d <: Dim], S, P <: HList, In <: Dim, Out <: Dim](
    f: ParametrisedDifferentiable[P, M[S, In], M[S, Out]]
  )(
    eps: S
  , cost: MatrixFunction2[M, S]
  )(
    implicit
      mat: MatrixCalculus[M, S]
    , pmod: Module[P, S]
    , mNorm: DimNorm[Out, S]
    , rMultGroup: MultiplicativeGroup[S]
    // not useful... just to display things in debug mode
    // , showP: Show[P]
    // , showA: Show[M[S, In]]
  ): HLearn[P, M[S, In], M[S, Out]] = new HLearn[P, M[S, In], M[S, Out]] {

      def implement(params: P)(a: M[S, In]) = {
        f(params)(a)
      }

      def update(params: P)(a: M[S, In], b: M[S, Out]) = {
        //UI(p, a, b) ≔ p − ε∇pEI(p, a, b)
        val estim: M[S, Out] = f(params)(a)
        val err: M[S, Out] = cost[Out].diff(estim)(b)
        val diffPerP: P = f.diffPerP(params)(a)(err)
        // val paramsUpd = mat.times(mat.mult(diffPerP, err), rMultGroup.times(eps, mNorm.norm))
        val paramsUpd: P = pmod.timesr(diffPerP, rMultGroup.times(eps, mNorm.norm))
        // println(s"mult:${rMultGroup.times(eps, mNorm.norm)} paramsUpd:${showP.show(paramsUpd)}")
        pmod.minus(params, paramsUpd)
      }

      def request(params: P)(a: M[S, In], b: M[S, Out]) = {
        // rI(p, a, b) ≔ fa((1/α(m))*∇aEI(p, a, b))
        val estim: M[S, Out] = f(params)(a)
        val err: M[S, Out] = cost[Out].diff(estim)(b)
        val diffPerA: M[S, In] = f.diffPerA(params)(a)(err)
        val aUpdate = mat.times(diffPerA, rMultGroup.reciprocal(mNorm.norm))
        // println(s"aUpdate:${showA.show(aUpdate)}")
        cost[In].diffInvert(a)(aUpdate)
      }
  }


  def NNetLayer2ParaFn[M[a, d <: Dim], S, W <: Dim2[_, _], In <: Dim2[_, _], Out <: Dim2[_, _]](
    layer: NNetLayer[M, S, W, In, Out]
  )(implicit
    mat: MatrixCalculus[M, S]
  ): ParametrisedDifferentiable[M[S, W] :: HNil, M[S, In], M[S, Out]] =
    new ParametrisedDifferentiable[M[S, W] :: HNil, M[S, In], M[S, Out]] {
      def apply(params: M[S, W] :: HNil)(a: M[S, In]): M[S, Out] = {
        layer.activation(layer.body(params.head)(a))
      }

      def diffPerP(params: M[S, W] :: HNil)(a: M[S, In])(e: M[S, Out]): M[S, W] :: HNil = {
        val s: M[S, Out] = layer.activation.diff(layer.body(params.head)(a))
        val t: M[S, W] = layer.body.diffPerP(params.head)(a)(mat.hadamard(e, s))
        t :: HNil
      }

      def diffPerA(params: M[S, W] :: HNil)(a: M[S, In])(e: M[S, Out]): M[S, In] = {
        val s: M[S, Out] = layer.activation.diff(layer.body(params.head)(a))
        layer.body.diffPerA(params.head)(a)(mat.hadamard(e, s))
      }

    }


  trait Layers2HLearn[
    M[a, d <: Dim], S, In <: Dim2[_, _], Out <: Dim2[_, _]
  , WS <: HList, L <: Layers[M, S, WS, In, Out]
  ] {

    def convert2Learn(ls: L)(
      eps: S
    , cost: MatrixFunction2[M, S]
    ): HLearn[WS, M[S, In], M[S, Out]]

  }

  object Layers2HLearn {
    implicit def one[M[a, d <: Dim], S, W <: Dim2[_, _], In <: Dim2[_, _], Out <: Dim2[_, _]](implicit
      mat: MatrixCalculus[M, S]
    , pmod: Module[M[S, W] :: HNil, S]
    , mNorm: DimNorm[Out, S]
    , rMultGroup: MultiplicativeGroup[S]
    ) = new Layers2HLearn[
        M, S, In, Out
      , M[S, W] :: HNil
      , ConsLayer[M, S, W, In, Out, Out, HNil, NNil[M, S, Out]]
    ] {
      def convert2Learn(
        ls: ConsLayer[M, S, W, In, Out, Out, HNil, NNil[M, S, Out]]
      )(
        eps: S
      , cost: MatrixFunction2[M, S]
      ): HLearn[M[S, W] :: HNil, M[S, In], M[S, Out]] = {
        ParaFn2Learn(NNetLayer2ParaFn(ls.h))(eps, cost)
      }

    }

    implicit def cons[
      M[a, d <: Dim], S, W <: Dim2[_, _], In <: Dim2[_, _], Hidden <: Dim2[_, _], Out <: Dim2[_, _]
    , WS <: HList
    , L <: Layers[M, S, WS, Hidden, Out]
    ](implicit
      mat: MatrixCalculus[M, S]
    , pmod: Module[M[S, W] :: HNil, S]
    , mNorm: DimNorm[Hidden, S]
    , rMultGroup: MultiplicativeGroup[S]
    , next: Layers2HLearn[M, S, Hidden, Out, WS, L]
    // , prepend : Prepend.Aux[M[S, H x InR] :: HNil, CS, M[S, H x InR] :: CS]
    // , take: Take.Aux[M[S, W] :: CS, Nat._1, M[S, H x InR] :: HNil]
    // , drop: Drop.Aux[M[S, H x InR] :: CS, Nat._1, CS]
    ) = new Layers2HLearn[
        M, S, In, Out
      , M[S, W] :: WS
      , ConsLayer[M, S, W, In, Hidden, Out, WS, L]
    ] {
      def convert2Learn(
        ls: ConsLayer[M, S, W, In, Hidden, Out, WS, L]
      )(
        eps: S
      , cost: MatrixFunction2[M, S]
      ): HLearn[M[S, W] :: WS, M[S, In], M[S, Out]] = {
        ParaFn2Learn(NNetLayer2ParaFn(ls.h))(eps, cost).andThen(
          next.convert2Learn(ls.t)(eps, cost)
        )
      }
    }

  }

}


  // def ParaFn2Learn[M[a, d <: Dim], S, P, In <: Dim, Out <: Dim](
  //   f: ParametrisedDifferentiable[P, M[S, In], M[S, Out]]
  // )(
  //   eps: S
  // , costM: PartialDifferentiable2[M[S, Out], 1]
  // , costN: PartialDifferentiableInvertible2[M[S, In], 1]
  // )(
  //   implicit
  //     mat: MatrixCalculus[M, S]
  //   , pmod: Module[P, S]
  //   , mNorm: DimNorm[Out, S]
  //   , rMultGroup: MultiplicativeGroup[S]
  //   // not useful... just to display things in debug mode
  //   // , showP: Show[P]
  //   // , showA: Show[M[S, In]]
  // ): Learn.Aux[P, M[S, In], M[S, Out]] = new Learn[M[S, In], M[S, Out]] {
  //     type Params   = P

  //     def implement(params: P)(a: M[S, In]) = {
  //       f(params)(a)
  //     }

  //     def update(params: P)(a: M[S, In], b: M[S, Out]) = {
  //       //UI(p, a, b) ≔ p − ε∇pEI(p, a, b)
  //       val estim: M[S, Out] = f(params)(a)
  //       val err: M[S, Out] = costM.diff(estim)(b)
  //       val diffPerP: P = f.diffPerP(params)(a)(err)
  //       // val paramsUpd = mat.times(mat.mult(diffPerP, err), rMultGroup.times(eps, mNorm.norm))
  //       val paramsUpd: P = pmod.timesr(diffPerP, rMultGroup.times(eps, mNorm.norm))
  //       // println(s"mult:${rMultGroup.times(eps, mNorm.norm)} paramsUpd:${showP.show(paramsUpd)}")
  //       pmod.minus(params, paramsUpd)
  //     }

  //     def request(params: P)(a: M[S, In], b: M[S, Out]) = {
  //       // rI(p, a, b) ≔ fa((1/α(m))*∇aEI(p, a, b))
  //       val estim: M[S, Out] = f(params)(a)
  //       val err: M[S, Out] = costM.diff(estim)(b)
  //       val diffPerA: M[S, In] = f.diffPerA(params)(a)(err)
  //       val aUpdate = mat.times(diffPerA, rMultGroup.reciprocal(mNorm.norm))
  //       // println(s"aUpdate:${showA.show(aUpdate)}")
  //       costN.diffInvert(a)(aUpdate)
  //     }
  // }



  // def NNetLayer2ParaFn[M[a, d <: Dim], S, InR <: XInt, OutR <: XInt, OutC <: XInt](
  //   layer: NNetLayer[M, S, InR, OutR, OutC]
  // )(implicit
  //   mat: MatrixCalculus[M, S]
  // ): EuclideanParametrisedFunction[M, S, OutR x InR, InR x OutC, OutR x OutC] =
  //   new EuclideanParametrisedFunction[M, S, OutR x InR, InR x OutC, OutR x OutC] {
  //     def apply(params: M[S, OutR x InR])(a: M[S, InR x OutC]): M[S, OutR x OutC] = {
  //       layer.activation(layer.body(params)(a))
  //     }

  //     def diffPerP(params: M[S, OutR x InR])(a: M[S, InR x OutC])(e: M[S, OutR x OutC]): M[S, OutR x InR] = {
  //       val s: M[S, OutR x OutC] = layer.activation.diff(layer.body(params)(a))
  //       val t: M[S, OutR x InR] = layer.body.diffPerP(params)(a)(mat.hadamard(e, s))
  //       t
  //     }

  //     def diffPerA(params: M[S, OutR x InR])(a: M[S, InR x OutC])(e: M[S, OutR x OutC]): M[S, InR x OutC] = {
  //       val s: M[S, OutR x OutC] = layer.activation.diff(layer.body(params)(a))
  //       layer.body.diffPerA(params)(a)(mat.hadamard(e, s))
  //     }

  //   }


  // def NNetLayer2ParaFn2[M[a, d <: Dim], S, InR <: XInt, OutR <: XInt, OutC <: XInt](
  //   layer: NNetLayer[M, S, InR, OutR, OutC]
  // )(implicit
  //   mat: MatrixCalculus[M, S]
  // ): ParametrisedDifferentiable[M[S, OutR x InR] :: HNil, M[S, InR x OutC], M[S, OutR x OutC]] =
  //   new ParametrisedDifferentiable[M[S, OutR x InR] :: HNil, M[S, InR x OutC], M[S, OutR x OutC]] {
  //     def apply(params: M[S, OutR x InR] :: HNil)(a: M[S, InR x OutC]): M[S, OutR x OutC] = {
  //       layer.activation(layer.body(params.head)(a))
  //     }

  //     def diffPerP(params: M[S, OutR x InR] :: HNil)(a: M[S, InR x OutC])(e: M[S, OutR x OutC]): M[S, OutR x InR] :: HNil = {
  //       val s: M[S, OutR x OutC] = layer.activation.diff(layer.body(params.head)(a))
  //       val t: M[S, OutR x InR] = layer.body.diffPerP(params.head)(a)(mat.hadamard(e, s))
  //       t :: HNil
  //     }

  //     def diffPerA(params: M[S, OutR x InR] :: HNil)(a: M[S, InR x OutC])(e: M[S, OutR x OutC]): M[S, InR x OutC] = {
  //       val s: M[S, OutR x OutC] = layer.activation.diff(layer.body(params.head)(a))
  //       layer.body.diffPerA(params.head)(a)(mat.hadamard(e, s))
  //     }

  //   }
  // trait Layers2HLearn[
  //   M[a, d <: Dim], S, InR <: XInt, OutR <: XInt, OutC <: XInt
  // , CS <: HList
  // , L <: Layers[M, S, InR, OutR, OutC, CS]
  // ] {

  //   def convert2Learn(ls: L)(
  //     eps: S
  //   , cost: MatrixFunction2[M, S]
  //   ): HLearn[CS, M[S, InR x OutC], M[S, OutR x OutC]]

  // }

  // object Layers2HLearn {
  //   implicit def one[M[a, d <: Dim], S, InR <: XInt, OutR <: XInt, OutC <: XInt](implicit
  //     mat: MatrixCalculus[M, S]
  //   , pmod: Module[M[S, OutR x InR] :: HNil, S]
  //   , mNorm: DimNorm[OutR x OutC, S]
  //   , rMultGroup: MultiplicativeGroup[S]
  //   // // not useful... just to display things in debug mode
  //   // , showP: Show[M[S, OutR x InR] :: HNil]
  //   // , showA: Show[M[S, InR x OutC]]
  //   ) = new Layers2HLearn[
  //       M, S, InR, OutR, OutC
  //     , M[S, OutR x InR] :: HNil
  //     , ConsLayer[M, S, InR, OutR, OutR, OutC, HNil, NNil[M, S, OutR, OutC]]
  //   ] {
  //     def convert2Learn(
  //       ls: ConsLayer[M, S, InR, OutR, OutR, OutC, HNil, NNil[M, S, OutR, OutC]]
  //     )(
  //       eps: S
  //     , cost: MatrixFunction2[M, S]
  //     ): HLearn[M[S, OutR x InR] :: HNil, M[S, InR x OutC], M[S, OutR x OutC]] = {
  //       ParaFn2Learn3(NNetLayer2ParaFn2(ls.h))(eps, cost)
  //     }

  //   }

  //   implicit def cons[
  //     M[a, d <: Dim], S, InR <: XInt, H <: XInt, OutR <: XInt, OutC <: XInt
  //   , CS <: HList
  //   , L <: Layers[M, S, H, OutR, OutC, CS]
  //   ](implicit
  //     mat: MatrixCalculus[M, S]
  //   , pmod: Module[M[S, H x InR] :: HNil, S]
  //   , mNorm: DimNorm[H x OutC, S]
  //   , rMultGroup: MultiplicativeGroup[S]
  //   , next: Layers2HLearn[M, S, H, OutR, OutC, CS, L]
  //   , prepend : Prepend.Aux[M[S, H x InR] :: HNil, CS, M[S, H x InR] :: CS]
  //   , take: Take.Aux[M[S, H x InR] :: CS, Nat._1, M[S, H x InR] :: HNil]
  //   , drop: Drop.Aux[M[S, H x InR] :: CS, Nat._1, CS]
  //   ) = new Layers2HLearn[
  //       M, S, InR, OutR, OutC
  //     , M[S, H x InR] :: CS
  //     , ConsLayer[M, S, InR, H, OutR, OutC, CS, L]
  //   ] {
  //     def convert2Learn(
  //       ls: ConsLayer[M, S, InR, H, OutR, OutC, CS, L]
  //     )(
  //       eps: S
  //     , cost: MatrixFunction2[M, S]
  //     ): HLearn[M[S, H x InR] :: CS, M[S, InR x OutC], M[S, OutR x OutC]] = {
  //       ParaFn2Learn3(NNetLayer2ParaFn2(ls.h))(eps, cost).andThen(
  //         next.convert2Learn(ls.t)(eps, cost)
  //       )
  //     }

  //   }


  // }

