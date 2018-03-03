package neurocat
package test

import minitest._
import cats._
import cats.implicits._

import org.nd4j.linalg.factory.Nd4j

import nd4j._
import org.nd4j.linalg.api.ops.impl.transforms.Sigmoid
import spire.std.double._

import idag._
import idag.nd4j._
// import neurocat.nd4j._


object MnistTest extends SimpleTestSuite {
  val dsl = new DiffDagDsl[Double, ND4JAlgebra]
                with nd4j.DiffDsl[Double, ND4JAlgebra] {}
  import dsl._

  test("mnist") {
    
    import nd4j.L2._
    weightMat["s", 728, 1, 30]
  }

}