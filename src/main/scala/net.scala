package neurocat

import shapeless.{HList, HNil, ::}

import singleton.ops._


/* a neural network in euclidean space is defined by layers composed together
 * each layer has:
 * - an activation differentiable function (like sigmoid)
 * - an output function (W * X) which is a differentiable parametrised function
 */
class NNetLayer[M[a, d <: Dim], R, InR <: XInt, OutR <: XInt, OutC <: XInt](
  val activation: Differentiable[M[R, OutR x OutC], M[R, OutR x OutC]]
, val output: ParametrisedDifferentiable[M[R, OutR x InR], M[R, InR x OutC], M[R, OutR x OutC]]
)



class NNetLayerBuilder[M[a, d <: Dim], R, InR <: XInt, OutR <: XInt, OutC <: XInt]() {
  def build(
    activation: Differentiable[M[R, OutR x OutC], M[R, OutR x OutC]]
  , output: ParametrisedDifferentiable[M[R, OutR x InR], M[R, InR x OutC], M[R, OutR x OutC]]
  ) = new NNetLayer[M, R, InR, OutR, OutC](
    activation
  , output
  )
}

/** Don't be frightened, this code is ugly due to types but very mechanical to write */
object NNetLayerBuilder {

  def apply[M[a, d <: Dim], R, DIn <: Dim2[_, _], DOut <: Dim2[_, _]](
    implicit same: Dim2SameCol[DIn, DOut]
  ) = new NNetLayerBuilder[M, R, same.In, same.OutR, same.OutC]()
}

sealed trait Layers[M[a, d <: Dim], R, InR <: XInt, OutR <: XInt, OutC <: XInt, CS <: HList] {
  // this function could be externalized but added here for convenience in implementing Layers
  def apply(weights: CS)(in: M[R, InR x OutC]): M[R, OutR x OutC]

}

final case class NNil[M[a, d <: Dim], R, InR <: XInt, OutC <: XInt]()
  extends Layers[M, R, InR, InR, OutC, HNil] {

  def apply(weights: HNil)(in: M[R, InR x OutC]): M[R, InR x OutC] = in

  def ::[InR2 <: XInt](
    z: NNetLayer[M, R, InR2, InR, OutC]
  ): ConsLayer[M, R, InR2, InR, InR, OutC, HNil, NNil[M, R, InR, OutC]] =
    ConsLayer[M, R, InR2, InR, InR, OutC, HNil, NNil[M, R, InR, OutC]](z, this)
}

final case class ConsLayer[
  M[a, d <: Dim], R, InR <: XInt, I <: XInt, OutR <: XInt, OutC <: XInt, CS <: HList
, +L <: Layers[M, R, I, OutR, OutC, CS]
] private (
  h: NNetLayer[M, R, InR, I, OutC]
, t: L
) extends Layers[M, R, InR, OutR, OutC, M[R, I x InR] :: CS] {

  def apply(weights: M[R, I x InR] :: CS)(in: M[R, InR x OutC]): M[R, OutR x OutC] = {
    // could use ParaFn compose too...
    t(weights.tail)(h.output(weights.head)(in))
  }

}

final class LayersOps[
  M[a, d <: Dim], R, InR <: XInt, I <: XInt, OutR <: XInt, OutC <: XInt, CS <: HList
, L <: Layers[M, R, I, OutR, OutC, CS]
](l : ConsLayer[M, R, InR, I, OutR, OutC, CS, L]) {

  // trick to have nice message
  def ::[InR2 <: XInt](h: NNetLayer[M, R, InR2, InR, OutC]) =
    ConsLayer[
      M, R, InR2, InR, OutR, OutC, M[R, I x InR] :: CS
    , ConsLayer[M, R, InR, I, OutR, OutC, CS, L]
    ](h, l)

  def toLearn(
    eps: R
  , cost: MatrixFunction2[M, R]
  )(
    implicit conv: Layers2HLearn[M, R, InR, OutR, OutC, M[R, I x InR] :: CS, ConsLayer[M, R, InR, I, OutR, OutC, CS, L]]
  ): HLearn[M[R, I x InR] :: CS, M[R, InR x OutC], M[R, OutR x OutC]] = conv.convert2Learn(l)(eps, cost)
}

object Layers {

  implicit def layersOps[
    M[a, d <: Dim], R, InR <: XInt, I <: XInt, OutR <: XInt, OutC <: XInt, CS <: HList
  , L <: Layers[M, R, I, OutR, OutC, CS]
  ](l: ConsLayer[M, R, InR, I, OutR, OutC, CS, L]): LayersOps[M, R, InR, I, OutR, OutC, CS, L] = new LayersOps[M, R, InR, I, OutR, OutC, CS, L](l)

}




case class NNetLayer0[M[a, d <: Dim], R, W <: Dim2[_, _], In <: Dim2[_, _], Out <: Dim2[_, _]](
  val activation: Differentiable[M[R, Out], M[R, Out]]
, val output: ParametrisedDifferentiable[M[R, W], M[R, In], M[R, Out]]
)

trait Dim4NNetLayer[In <: Dim2[_, _], Out <: Dim2[_, _]] {
  type W <: Dim2[_, _]
}

object Dim4NNetLayer {
  implicit def d2[InR <: XInt, OutR <: XInt, OutC <: XInt] =
    new Dim4NNetLayer[InR x OutC, OutR x OutC] {
      type W = OutR x InR
    }
}

// object NNetLayer0 {
//   def apply[M[a, d <: Dim], R, In <: Dim2[_, _], Out <: Dim2[_, _]](
//     activation: Differentiable[M[R, Out], M[R, Out]]
//   , output: ParametrisedDifferentiable[M[R, W], M[R, In], M[R, Out]]
//   )(implicit
//     dims: Dim4NNetLayer[W, In, Out]
//   ) = new NNetLayer0[M, R, W, In, Out](
//     activation
//   , output
//   )
// }

class NNetLayerBuilder0[M[a, d <: Dim], R, W <: Dim2[_, _], In <: Dim2[_, _], Out <: Dim2[_, _]]() {
  def build(
    activation: Differentiable[M[R, Out], M[R, Out]]
  , output: ParametrisedDifferentiable[M[R, W], M[R, In], M[R, Out]]
  ) = NNetLayer0[M, R, W, In, Out](
    activation
  , output
  )
}

object NNetLayerBuilder0 {

  def apply[M[a, d <: Dim], R, In <: Dim2[_, _], Out <: Dim2[_, _]](
    implicit d4net: Dim4NNetLayer[In, Out]
  ) = new NNetLayerBuilder0[M, R, d4net.W, In, Out]
}


sealed trait Layers0[M[a, d <: Dim], R, WS <: HList, In <: Dim2[_, _], Out <: Dim2[_, _]] {
  // this function could be externalized but added here for convenience in implementing Layers
  def apply(weights: WS)(in: M[R, In]): M[R, Out]
}


final case class NNil0[M[a, d <: Dim], R, In <: Dim2[_, _]]()
  extends Layers0[M, R, HNil, In, In] {

  def apply(weights: HNil)(in: M[R, In]): M[R, In] = in

  def ::[W2 <: Dim2[_, _], In2 <: Dim2[_, _]](
    z: NNetLayer0[M, R, W2, In2, In]
  ): ConsLayer0[M, R, W2, In2, In, In, HNil, NNil0[M, R, In]] =
    ConsLayer0[M, R, W2, In2, In, In, HNil, NNil0[M, R, In]](z, this)
}

final case class ConsLayer0[
  M[a, d <: Dim], R, W <: Dim2[_, _], In <: Dim2[_, _], Hidden <: Dim2[_, _], Out <: Dim2[_, _]
, WS <: HList
, L <: Layers0[M, R, WS, Hidden, Out]
] private (
  val h: NNetLayer0[M, R, W, In, Hidden]
, val t: L
) extends Layers0[M, R, M[R, W] :: WS, In, Out] {

  def apply(weights: M[R, W] :: WS)(in: M[R, In]): M[R, Out] = {
    // could use ParaFn compose too...
    t(weights.tail)(h.output(weights.head)(in))
  }

}



final class Layers0Ops[
  M[a, d <: Dim], R, W <: Dim2[_, _], In <: Dim2[_, _], Hidden <: Dim2[_, _], Out <: Dim2[_, _]
, WS <: HList, L <: Layers0[M, R, WS, Hidden, Out]
](l : ConsLayer0[M, R, W, In, Hidden, Out, WS, L]) {

  // trick to have nice message
  def ::[W2 <: Dim2[_, _], In2 <: Dim2[_, _]](h: NNetLayer0[M, R, W2, In2, In]) =
    ConsLayer0[
      M, R, W2, In2, In, Out
    , M[R, W] :: WS, ConsLayer0[M, R, W, In, Hidden, Out, WS, L]
    ](h, l)

  def toLearn(
    eps: R
  , cost: MatrixFunction2[M, R]
  )(
    implicit conv: Layers2HLearn0[M, R, In, Out, M[R, W] :: WS, ConsLayer0[M, R, W, In, Hidden, Out, WS, L]]
  ): HLearn[M[R, W] :: WS, M[R, In], M[R, Out]] = conv.convert2Learn(l)(eps, cost)
}

object Layers0 {

  implicit def layers0Ops[
    M[a, d <: Dim], R, W <: Dim2[_, _], In <: Dim2[_, _], Hidden <: Dim2[_, _], Out <: Dim2[_, _]
  , WS <: HList, L <: Layers0[M, R, WS, Hidden, Out]
  ](l: ConsLayer0[M, R, W, In, Hidden, Out, WS, L]): Layers0Ops[M, R, W, In, Hidden, Out, WS, L] = new Layers0Ops[M, R, W, In, Hidden, Out, WS, L](l)

}

