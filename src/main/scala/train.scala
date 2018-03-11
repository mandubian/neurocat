package neurocat

import singleton.ops._
import cats.Cartesian
import shapeless.HList


trait Trainer[
  DataSet[row, nb]
] {
  def train[Params <: HList, In, Out, NbSamples <: XInt : SafeInt](
    learner: HLearn[Params, In, Out]
  )(
    initParams: Params
  , trainingData: DataSet[(In, Out), NbSamples]
  ): Params
}
