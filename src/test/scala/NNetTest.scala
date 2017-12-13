package neurocat

import minitest._
import cats._
import cats.implicits._

import org.nd4j.linalg.factory.Nd4j

import nd4j._
import org.nd4j.linalg.api.ops.impl.transforms.Sigmoid
import spire.std.double._


object NNetTest extends SimpleTestSuite {
  test("simple neural layer to parametrised function") {
    val weights = Mat.randomD2[Double, 1 x 3](min = 0.0, max = 1.0)

    println(weights.show)

    val netLayer = NNetLayerBuilder[Mat, Double, 3 x 1, 1 x 1].build(
      output     = Output.Dense
    , activation = Activation.Sigmoid
    )

    val x = Mat.columnVector[Double, 3](Array(0.0, 1.0, 0.0))
    val y = netLayer.activation(netLayer.output(weights)(x))

    println(s"x:${x.show} y:${y.show}")

    val parafn = Neurocat.NNetLayer2ParaFn(netLayer)

    val y2 = parafn(weights)(x)

    println(s"x:${x.show} y2:${y2.show}")

    // dummy test for now
    assertEquals(2, 2)
  }

  test("2-layers neural network to learner conversion (XOR)") {
    // 1st layer
    //                            (Matrix constrained by size)
    //                            |    (Scala type in the matrix)
    //                            |    |         (Input neurons size)
    //                            |    |         |      (Output neurons size)
    //                            |    |         |      |
    //                            ˅    ˅         ˅      ˅
    val layer1 = NNetLayerBuilder[Mat, Double, 2 x 1, 2 x 1].build(
      output     = Output.Dense
    , activation = Activation.Sigmoid
    )

    // 2nd layer
    val layer2 = NNetLayerBuilder[Mat, Double, 2 x 1, 1 x 1].build(
      output     = Output.Dense
    , activation = Activation.Sigmoid
    )

    // convert layer 1 into parametrised function then into learner 
    val parafn1 = Neurocat.NNetLayer2ParaFn(layer1)
    val learn1 = Neurocat.ParaFn2Learn(parafn1)(0.1, Loss.L2, Loss.L2)

    // convert layer 2 into parametrised function then into learner 
    val parafn2 = Neurocat.NNetLayer2ParaFn(layer2)
    val learn2 = Neurocat.ParaFn2Learn(parafn2)(0.1, Loss.L2, Loss.L2)

    // compose both learners into one single learner
    val learn = learn1.andThen(learn2)
    /* Please remark that type of learn is:
     *                         Params type is the tuple of both layers params
     *                              |                      (Input neurons size from layer1)
     *                              |                      |                   (Output neurons size from layer2)
     *                              |                      |                   |
     *                              ˅                      ˅                   ˅
     * Learn.Aux[(Mat[Double, 2 x 2], Mat[Double, 1 x 2]), Mat[Double, 2 x 1], Mat[Double, 1 x 1]]
     * 
     * There is a hidden layer 2 x 1 in this learner
     */
      

    // Train this learner
    // Input Training Samples
    val trainX = Mat.fromArrays[Double, 4 x 2](Array(
      Array(0, 0)
    , Array(0, 1)
    , Array(1, 0)
    , Array(1, 1)
    ))

    // Output Training Samples
    val trainY = Mat.fromArrays[Double, 4 x 1](Array(
      Array(0)
    , Array(1)
    , Array(1)
    , Array(0)
    ))

    // layer1 initial weights
    val weights1 = Mat.randomD2[Double, 2 x 2](min = 0.0, max = 1.0)
    // val weights1 = Mat.fromArrays[Double, 2 x 2](Array(
    //   Array(1, 1)
    // , Array(-1, -1)
    // ))

    // layer2 initial weights
    val weights2 = Mat.randomD2[Double, 1 x 2](min = 0.0, max = 1.0)
    // val weights2 = Mat.fromArrays[Double, 1 x 2](Array(
    //   Array(1, 1)
    // ))

    // Train
    val trainedParams = Mat.train(learn)((weights1, weights2), trainX, trainY)
    println(s"trainedParams:${trainedParams.show}")

    // Get some estimated from trainedParams
    val x = Mat.columnVector[Double, 2](Array(0, 1.0))
    val y = learn.implement(trainedParams)(x)

    println(s"x:${x.show} y:${y.show}")

    val x2 = Mat.columnVector[Double, 2](Array(1, 1))
    val y2 = learn.implement(trainedParams)(x2)

    println(s"x2:${x2.show} y:${y2.show}")

    // val ws = learn.update(trainedParams)(x)(y)

    assertEquals(2, 2)
  }

  test("Very big neuron layers that compiles fast") {
    val layer1 = NNetLayerBuilder[Mat, Double, 20000 x 1, 30000 x 1].build(
      output     = Output.Dense
    , activation = Activation.Sigmoid
    )

    // 2nd layer
    val layer2 = NNetLayerBuilder[Mat, Double, 30000 x 1, 100000 x 1].build(
      output     = Output.Dense
    , activation = Activation.Sigmoid
    )

    // convert layer 1 into parametrised function then into learner 
    val parafn1 = Neurocat.NNetLayer2ParaFn(layer1)
    val learn1 = Neurocat.ParaFn2Learn(parafn1)(0.1, Loss.L2, Loss.L2)

    // convert layer 2 into parametrised function then into learner 
    val parafn2 = Neurocat.NNetLayer2ParaFn(layer2)
    val learn2 = Neurocat.ParaFn2Learn(parafn2)(0.1, Loss.L2, Loss.L2)

    // compose both learners into one single learner
    val learn = learn1.andThen(learn2)

    val weights1 = Mat.randomD2[Double, 30000 x 20000](min = 0.0, max = 1.0)
    val weights2 = Mat.randomD2[Double, 100000 x 30000](min = 0.0, max = 1.0)
  
    // compute a simple estimation with current params hypothesis
    val x = Mat.randomD2[Double, 20000 x 1](min = 0.0, max = 1.0)
    // you need a lot of memory locally or in GPU here ;)
    // val y = learn.implement((weights1, weights2))(x)

  }
}