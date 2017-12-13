package neurocat

import minitest._
import cats._
import cats.implicits._

import org.nd4j.linalg.factory.Nd4j

import nd4j._
import org.nd4j.linalg.api.ops.impl.transforms.Sigmoid
import spire.std.double._


object MatTest extends SimpleTestSuite {
  test("simple matrix") {
    val x  = Mat.randomD2[Double, 3 x 2](min = 1.0, max = 10.0)
    val y  = Mat.randomD2[Double, 2 x 4](min = 1.0, max = 10.0)
    val y2 = Mat.randomD2[Double, 3 x 4](min = 1.0, max = 10.0)

    val matcalc = implicitly[MatrixCalculus[Mat, Double]]

    // Compiles
    val z /*: Mat[Double, 3 x 4]*/ = matcalc.mult(x, y)

    ////////////////////////////////////////////////////////////////////////
    // DOESN'T COMPILE... not multipliable dimensions
    // val z2: Mat[Double, 2 x 4] = matcalc.mult(x, y)
    // [error]   val z: Mat[Double, 2 x 4] = matcalc.mult(x, y)
    // [error]       ^
    // [error]  found   : neurocat.nd4j.Mat[Double,neurocat.Dim2[3,4]]
    // [error]  required: neurocat.nd4j.Mat[Double,neurocat.x[2,4]]
    // [error]     (which expands to)  neurocat.nd4j.Mat[Double,neurocat.Dim2[2,4]]
    // [error]   val z: Mat[Double, 2 x 4] = matcalc.mult(x, y)
    // [error]                                           ^

    ////////////////////////////////////////////////////////////////////////
    // DOESN'T COMPILE... not multipliable dimensions
    // val z3 = matcalc.mult(x, y2)
    // MatTest.scala:34: could not find implicit value for parameter mult: neurocat.DimMult[neurocat.x[3,2],neurocat.x[3,4]]
    // [error]   val z2 = matcalc.mult(x, y2)
    // [error]                        ^

    ////////////////////////////////////////////////////////////////////////
    // DOESN'T COMPILE ... not same dimensions (error is a bit ugly for now :D)
    // val z4 = matcalc.minus(x, y)

    // MatTest.scala:40: type mismatch;
    // [error]  found   : neurocat.nd4j.Mat[Double,neurocat.x[3,2]]
    // [error]     (which expands to)  neurocat.nd4j.Mat[Double,neurocat.Dim2[3,2]]
    // [error]  required: neurocat.nd4j.Mat[Double,neurocat.Dim2[_ >: 2 with 3 <: Int, _ >: 4 with 2 <: Int]]
    // [error] Note: neurocat.x[3,2] <: neurocat.Dim2[_ >: 2 with 3 <: Int, _ >: 4 with 2 <: Int], but class Mat is invariant in type D.
    // [error] You may wish to define D as +D instead. (SLS 4.5)
    // [error]     val z4 = matcalc.minus(x, y)
    // [error]                            ^
    // [error] /Users/mandubian/workspaces/mandubian/neurocat/src/test/scala/MatTest.scala:40: type mismatch;
    // [error]  found   : neurocat.nd4j.Mat[Double,neurocat.x[2,4]]
    // [error]     (which expands to)  neurocat.nd4j.Mat[Double,neurocat.Dim2[2,4]]
    // [error]  required: neurocat.nd4j.Mat[Double,neurocat.Dim2[_ >: 2 with 3 <: Int, _ >: 4 with 2 <: Int]]
    // [error] Note: neurocat.x[2,4] <: neurocat.Dim2[_ >: 2 with 3 <: Int, _ >: 4 with 2 <: Int], but class Mat is invariant in type D.
    // [error] You may wish to define D as +D instead. (SLS 4.5)
    // [error]     val z4 = matcalc.minus(x, y)
    // [error]                               ^
  }

}