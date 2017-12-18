package neurocat

import singleton.ops._
import cats.Cartesian
import shapeless.HList


trait Trainer[
  DataSet[row, nb <: XInt]
] {
  def train[Params <: HList, In, Out, NbSamples <: XInt](
    learner: HLearn[Params, In, Out]
  )(
    initParams: Params
  , trainingData: DataSet[(In, Out), NbSamples]
  ): Params
}
