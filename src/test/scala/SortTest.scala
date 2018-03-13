package neurocat
package test

import idag._
import minitest._
import typeclasses._
import shapeless.{HNil, ::}
import algebra.ring.AdditiveSemigroup
import org.nd4j.linalg.factory.Nd4j
import singleton.ops._
import neurocat.nd4j._
import shapeless.labelled._
import shapeless._ ; import syntax.singleton._ ; import record._
import cats.Show
import cats.implicits._


object SortTest extends SimpleTestSuite {


  test("LearnedIndex neural network") {
    Nd4j.setDataType(org.nd4j.linalg.api.buffer.DataBuffer.Type.DOUBLE)

    // In current implementation, we need to import the Cost function (L2 here) able:
    // - to build type-independent element-wise cost functions in our context.
    // - which is differentiable & its differential is invertible
    // This is still a field of study in current code because I'd like get rid of that
    import nd4j.L2._

    // We manipulate differentiable DAG here that are just descriptions of parametrised functions (acyclic in theory):
    // DAG = description of (Parameters, Input(type A)) => Output(type B))
    // this function is differentiable partially per Parameters (gradP) and per Input (gradA)

    // 1 - Build the DSL
    //
    //     This DSL provides all operations to build structures in the category of parametrised differentaible DAG
    //     To be precise, ND4JAlgebra provides following grammars:
    //     - Parametrised differentiable DAG
    //     - ND4J/Neural network differentiable DAG
    val dsl = new DiffDagDsl[Double, Lambda[out[p, a, b] => ND4JAlgebra[out]]]
              with nd4j.DiffDsl[Double, Lambda[out[p, a, b] => ND4JAlgebra[out]]] {}
    // Import all DSL in scope
    import dsl._

    // 2 - Build a neural network model as a typesafe differentiable DAG
    //
    //     Here, we are in the category of parametrised differentiable DAG
    //     This DAG is just a pure data structure describing its process
    //     It can be manipulated/compiled/represented into another format
    //     using a transformation (between DAG category and another category or in DAG category)
    
    // hidden layer sizes based on Scala singleton-types
    val SZ: 32 = 32
    type SZ = SZ.type

    // This model looks basic but it is very interesting sample based on google paper
    // "The Case for Learned Index Structures" https://research.google.com/pubs/pub46518.html
    // and suggested by my friend & colleague Alex Tamborrino (@altamborrino) ;)
    val model = 
      weightMat["l1",  1 x 1, SZ x 1].build >>>   id >>>
      weightMat["l2", SZ x 1, SZ x 1].build >>> relu >>>
      weightMat["l3", SZ x 1, SZ x 1].build >>> relu >>>
      weightMat["l4", SZ x 1,  1 x 1].build >>>   id

    // l1/l2/l3/l4 are singleton identifiers for parameters linked to each layer.
    // Using this identified, imagine that we could share same parameters in 2 layers
    // like in a convolutional network!

    // 3 - Compile model into another category: here category of Strings :)
    //
    //     Compile model to String with ND4JStringCompiler
    val modelStr = model.compile[Lambda[(p, a, b) => String]](ND4JStringCompiler)
    println(s"Model: $modelStr")

    // Ok we want to learn things now...


    // 4 - Create a compiler from parametrised differentiable DAG category to Supervised Learner DAG Category
    //     equipped with a loss/cost function (& backpropagation)
    type LearnDag[p, a, b] = Dag[p, a, b, Double, ND4JAlgebra]
    val learnCompiler = LearnCompiler(learnRate = 0.006)

    // 5 - Compile parametrised differentiable DAG into a supervised parameter learner DAG
    //
    //     [Warning - Hard sentence now]
    //     We can build a supervised learner DAG from a parametrised differentiable DAG by:
    //     - taking the partial differential of the DAG per its parameters
    //     - compiling it into a DAG in the supervised learner category by equipping it with backpropagation
    //       relying on a differentiable invertible cost function (L2 imported above) & original partial 
    //       differential of the DAG per its input
    //     OUCHHHHH
    //     
    //     If you need more details, you can find some in fantastic paper
    //     " Backprop as Functor: A compositional perspective on supervised learning"
    //     https://arxiv.org/abs/1711.10455
    val learner = model.gradP.compile[LearnDag](learnCompiler)

    // 6 - Compile learner to String with ND4JStringCompiler
    //
    //     (this is easier)
    //     Don't look at the generated String :D
    //     It's quite huge & redundant because DAG has no rewrite rule & is not optimized at all for now ;)
    type StringOut[p, a, b] = String
    val learnerStr = learner.compile[StringOut](ND4JStringCompiler)
    println(s"learner: $learnerStr")

    // 7 - Compile supervised learner DAG to true Scala Parametrised Function relying on ND4J Backend
    //
    //     We still have a DAG which is just a description of process but it does nothing by itself
    //     So we need to convert into a true function that can be executed on a physical backend.
    //     In this case, we run in a Scala/Java VM using ND4J compiler
    val learnerFn = learner.compile(ND4JCompiler)


    // 8 - Build Training Data
    val trainX = DataSet[LearnedIndex.size](new Mat[Double, LearnedIndex.size x 1](LearnedIndex.data))
    val trainY = DataSet[LearnedIndex.size](new Mat[Double, LearnedIndex.size x 1](LearnedIndex.index))
    val trainXY = ProductDataSet(trainX, trainY)

    // 9 - Init weights
    // a kind of glorot_uniform to init weights
    val max1 = scala.math.sqrt(6.0 / (SZ + 1))
    val max2 = scala.math.sqrt(6.0 / (SZ + SZ))
    val layer1Weights =  Mat.randomD2[Double, SZ x 1](min = 0.0, max = max1)
    val layer2Weights =  Mat.randomD2[Double, SZ x SZ](min = 0.0, max = max2)
    val layer3Weights =  Mat.randomD2[Double, SZ x SZ](min = 0.0, max = max1)
    val layer4Weights =  Mat.randomD2[Double, 1 x SZ](min = 0.0, max = max1)

    val initWeights =
      ("l1" ->> layer1Weights) ::
      ("l2" ->> layer2Weights) ::
      ("l3" ->> layer3Weights) ::
      ("l4" ->> layer4Weights) ::
      shapeless.HNil

    // 9 - Train model
    //
    //     Just a naive loop for now going through data samples and returning updated weights
    val trainedWeights = 
      Trainer.naive[DataSet].train(learnerFn)(
        initWeights
      , trainXY
      )


    // 10 - Build a predictor by just compiling the model into Java function on ND4J
    //
    val predictor = model.compile(ND4JCompiler)

    // Does this work as is?
    // Not yet... it runs, it learns right? But I'm not sure what exactly does it learn for now :D
    // The concepts are here but there are many things to improve...


    // The rest is just a naive RMSE
    val layer1TrainedWeights :: layer2TrainedWeights :: layer3TrainedWeights :: layer4TrainedWeights :: HNil =
      trainedWeights

    println(s"layer1Weights       : ${layer1Weights.value}")
    println(s"layer1TrainedWeights: ${layer1TrainedWeights.value}")
    println(s"layer2Weights       : ${layer2Weights.value}")
    println(s"layer2TrainedWeights: ${layer2TrainedWeights.value}")
    println(s"layer3Weights       : ${layer3Weights.value}")
    println(s"layer3TrainedWeights: ${layer3TrainedWeights.value}")
    println(s"layer4Weights       : ${layer4Weights.value}")
    println(s"layer4TrainedWeights: ${layer4TrainedWeights.value}")

    var rmse = 0.0
    (0 until LearnedIndex.data.length).foreach { i =>
      val x = new Mat[Double, 1 x 1](LearnedIndex.data.getScalar(i))
      val y = new Mat[Double, 1 x 1](LearnedIndex.index.getScalar(i))
      val predY = predictor(trainedWeights, x)

      println(s"X: ${x.value} - y: ${y.value} - py: ${predY.value}")
      val pys = predY.value.getDouble(0)
      val ys = y.value.getDouble(0)
      rmse = rmse + (pys - ys) * (pys - ys)
    }
    rmse = scala.math.sqrt(rmse)
    println(s"RMSE : ${rmse}")

  }


}