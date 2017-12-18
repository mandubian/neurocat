package neurocat
package nd4j

import singleton.ops._
import org.nd4j.linalg.factory.Nd4j

object Output {
  def Dense[R, InR <: XInt, OutR <: XInt, OutC <: XInt] =
    new ParametrisedDifferentiable[
      Mat[R, OutR x InR]
    , Mat[R, InR x OutC]
    , Mat[R, OutR x OutC]
    ] {
      def apply(weights: Mat[R, OutR x InR])(in: Mat[R, InR x OutC]): Mat[R, OutR x OutC] = {
        new Mat[R, OutR x OutC](weights.value.mmul(in.value))
      }

      def diffPerP(weights: Mat[R, OutR x InR])(in: Mat[R, InR x OutC])(e: Mat[R, OutR x OutC]): Mat[R, OutR x InR] = {
        new Mat[R, OutR x InR](e.value.mmul(in.value.transpose()))
      }

      def diffPerA(weights: Mat[R, OutR x InR])(in: Mat[R, InR x OutC])(e: Mat[R, OutR x OutC]): Mat[R, InR x OutC] = {
        new Mat[R, InR x OutC](e.value.mmul(weights.value.transpose()))
      }
    }
}

object Activation {
  def Sigmoid[R, D <: Dim2[_, _]]: Differentiable[Mat[R, D], Mat[R, D]] =
    new Differentiable[Mat[R, D], Mat[R, D]] {
      def apply(in: Mat[R, D]) = new Mat[R, D](
        Nd4j.getExecutioner().execAndReturn(new org.nd4j.linalg.api.ops.impl.transforms.Sigmoid(in.value))
      )

      def diff(in: Mat[R, D]) = new Mat[R, D](
        Nd4j.getExecutioner().execAndReturn(new org.nd4j.linalg.api.ops.impl.transforms.SigmoidDerivative(in.value))
      )

    }


  def Id[R, D <: Dim2[_, _]](
    implicit rc: RowsCols[D]
  ): Differentiable[Mat[R, D], Mat[R, D]] =
    new Differentiable[Mat[R, D], Mat[R, D]] {
      def apply(in: Mat[R, D]) = in

      def diff(in: Mat[R, D]) = new Mat[R, D](
        Nd4j.ones(rc.rows, rc.cols)
      )

    }
}

object Loss {

  def localL2[R, D <: Dim] = new PartialDifferentiableInvertible2[Mat[R, D], 1] {
    def apply(x:Mat[R, D]) = { (y:Mat[R, D]) =>
      val scoreArr = x.value.rsub(y.value)
      new Mat[R, D](scoreArr.muli(scoreArr).muli(0.5))
    }

    def diff = { (x:Mat[R,D]) => (y:Mat[R,D]) =>
      new Mat(x.value.sub(y.value))
    }

    def diffInvert = { (x:Mat[R,D]) => (y:Mat[R,D]) =>
      new Mat(x.value.sub(y.value))
    }

  }

  def L2[R] = new MatrixFunction2[Mat, R] {
    def apply[D <: Dim]: PartialDifferentiableInvertible2[Mat[R, D], 1] = localL2[R, D]
  }
}