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

sealed trait Layers[M[a, d <: Dim], R, InR <: XInt, OutR <: XInt, OutC <: XInt, K <: XInt] {
  type CS <: HList

  // this function could be externalized but added here for convenience in implementing Layers
  def apply(weights: CS)(in: M[R, InR x OutC]): M[R, OutR x OutC]

}

object Layers {

  type Aux[M[a, d <: Dim], R, InR <: XInt, OutR <: XInt, OutC <: XInt, K <: XInt, CS0 <: HList] =
    Layers[M, R, InR, OutR, OutC, K] { type CS = CS0 }
}

final case class NNil[M[a, d <: Dim], R, Rows <: XInt, Cols <: XInt]() extends Layers[M, R, Rows, Rows, Cols, 0] {

  type CS = HNil

  def apply(weights: HNil)(in: M[R, Rows x Cols]): M[R, Rows x Cols] = in

  def ::[InR <: XInt](z: NNetLayer[M, R, InR, Rows, Cols]): OneLayer[M, R, InR, Rows, Cols] = OneLayer(z)
}

final case class OneLayer[M[a, d <: Dim], R, InR <: XInt, OutR <: XInt, OutC <: XInt](
  layer: NNetLayer[M, R, InR, OutR, OutC]
) extends Layers[M, R, InR, OutR, OutC, 1] {

  type CS = M[R, OutR x InR] :: HNil

  def apply(weights: M[R, OutR x InR] :: HNil)(in: M[R, InR x OutC]): M[R, OutR x OutC] =
    layer.output(weights.head)(in)

  def ::[InR2 <: XInt](
    h: NNetLayer[M, R, InR2, InR, OutC]
  ): ConsLayer[M, R, InR2, InR, OutR, OutC, 1, 2] =
    ConsLayer.build(h, this)
}

final case class ConsLayer[
    M[a, d <: Dim], R, InR <: XInt, I <: XInt, OutR <: XInt, OutC <: XInt, K <: XInt, SuccK <: XInt
] private (
  h: NNetLayer[M, R, InR, I, OutC]
, t: Layers[M, R, I, OutR, OutC, K]
) extends Layers[M, R, InR, OutR, OutC, SuccK] {
  type CS = M[R, Dim2[I, InR]] :: t.CS

  def apply(weights: M[R, Dim2[I, InR]] :: t.CS)(in: M[R, InR x OutC]): M[R, OutR x OutC] = {
    // could use ParaFn compose too...
    t(weights.tail)(h.output(weights.head)(in))
  }

  def ::[InR2 <: XInt, SuccK2 <: XInt](
    h: NNetLayer[M, R, InR2, InR, OutC]
  )(
    implicit op: OpAuxInt[SuccK + 1, SuccK2]
  ): ConsLayer[M, R, InR2, InR, OutR, OutC, SuccK, SuccK2] =
    ConsLayer.build(h, this)
}

object ConsLayer {
  def build[
    M[a, d <: Dim], R
  , InR <: XInt, I <: XInt, OutR <: XInt, OutC <: XInt, K <: XInt, SuccK <: XInt
  ](
    h: NNetLayer[M, R, InR, I, OutC], t: Layers[M, R, I, OutR, OutC, K]
  )(
    implicit op: OpAuxInt[K + 1, SuccK]
  ): ConsLayer[M, R, InR, I, OutR, OutC, K, SuccK] =
    new ConsLayer[M, R, InR, I, OutR, OutC, K, SuccK](h, t)
}
