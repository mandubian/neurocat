package neurocat

import singleton.ops._
import cats.Cartesian


trait Trainer[
  DataSet[row, nb <: XInt], S, Params, In <: Dim, Out <: Dim
] {
  def train[NbSamples <: XInt](
    learner: Learn.Aux[Params, In, Out]
  )(
    initParams: Params
  , trainingData: DataSet[(In, Out), NbSamples]
  ): Params
}
