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

  val dsl = new DiffDagDsl[Double, Lambda[out[p, a, b] => ND4JAlgebra[out]]]
  with nd4j.DiffDsl[Double, Lambda[out[p, a, b] => ND4JAlgebra[out]]] {}
  import dsl._

  test("lsort") {

    import nd4j.L2._

    val model = 
      ((dense[1, 1, 32] >>> id) >>> dense[32, 1, 1]) >>> relu
      /*((((dense[1, 1, 32] >>> id) >>>
        dense[32, 1, 32]) >>> relu) >>>
          dense[32, 1, 32] >>> relu) >>>
            dense[32, 1, 1] >>> id*/

    val learnCompiler = LearnCompiler(learnRate = 0.01)

    type Out[p, a, b] = Dag[p, a, b, Double, ND4JAlgebra]
    val learner = model.gradP.compile[Out](learnCompiler)
    val learnerFn = learner.compile(ND4JCompiler)

    val n = 100
    val data = Nd4j.rand(n, 1)
    val sortedData = Nd4j.sort(data.dup(), 0, true)

    val index = data.dup()
    val len = data.length
    (0 until len).foreach { i =>
      val d = index.getDouble(i)
      val Some(di) = (0 until len).find { j =>
        if(sortedData.getDouble(j) >= d) true
        else false
      }
      index.putScalar(i, di / len.toDouble)
    }

    println("data:" + data)
    println("sortedData:" + sortedData)
    println("index:" + index)

    val trainX = DataSet[100](new Mat[Double, 100 x 1](data))
    val trainY = DataSet[100](new Mat[Double, 100 x 1](index))
    val trainXY = ProductDataSet(trainX, trainY)

    val l1 =  Mat.randomD2[Double, 32 x 1](min = 0.0, max = 1.0)
    val l2 =  Mat.randomD2[Double, 32 x 32](min = 0.0, max = 1.0)
    val l3 =  Mat.randomD2[Double, 1 x 32](min = 0.0, max = 1.0)
    // val initWeights = l1 :: l2 :: l3 :: shapeless.HNil

    val l =  Mat.randomD2[Double, 1 x 1](min = 0.0, max = 1.0)
    val initWeights = l1 :: l3 :: shapeless.HNil

    // val trainedL1 :: trainedL2 :: trainedL3 :: HNil = 
    val trainedL1 :: trainedL3 :: HNil = 
      Trainer.naive[DataSet].train(learnerFn)(
        initWeights
      , trainXY.slice[0, 4]
      , beforeEach = { (p, in, out) =>
          println(">>>>> BEFORE")
          // println(s"BEFORE in0:${in.value.getScalar(0)} out0:${out.value.getScalar(0)} p0:${p.head.value.getScalar(0)}")
        }
      , afterEach = { (p, in, out) =>
          println(">>>>> AFTER")
          // val p1 :: p2 :: p3 :: HNil = p
          // println(s"AFTER in0:${in.value.getScalar(0)} out0:${out.value.getScalar(0)} p1:${p1.value.getScalar(0)}  p2:${p2.value.getScalar(0)}  p2:${p3.value.getScalar(0)}")
        }
      )

    println("trainedL1:" + l1.value.getScalar(0))
    // println("trainedL2:" + trainedL2.show)
    // println("trainedL3:" + trainedL3.show)

  }


}