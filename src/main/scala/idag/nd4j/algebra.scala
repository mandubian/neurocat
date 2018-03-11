package neurocat
package idag
package nd4j


import shapeless.{::, HNil}
import typeclasses._
import neurocat.nd4j._


trait Algebra[S, Alg[out[p, a, b]] <: nd4j.Algebra[S, Alg, out], Out[p, a, b]]
extends DagAlgebra[S, Alg, Out]
with Sigmoid.Algebra[S, Alg, Out]
with Relu.Algebra[S, Alg, Out]
with WeightMat.Algebra[S, Alg, Out]
with BiasMat.Algebra[S, Alg, Out]
with CostL2.Algebra[S, Alg, Out]
with CostL2Diff.Algebra[S, Alg, Out]
with CostL2DiffInvert.Algebra[S, Alg, Out]
with ScalarTimes.Algebra[S, Alg, Out]
with Minus.Algebra[S, Alg, Out]
with Func.Algebra[S, Alg, Out]
with Dense.Algebra[S, Alg, Out]



trait DiffAlgebra[S, Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out], Out[p, a, b]]
extends DiffDagAlgebra[S, Alg, Out]
with Algebra[S, Alg, Out]
with Sigmoid.DiffAlgebra[S, Alg, Out]
with Relu.DiffAlgebra[S, Alg, Out]
with WeightMat.DiffAlgebra[S, Alg, Out]
with BiasMat.DiffAlgebra[S, Alg, Out]
with Dense.DiffAlgebra[S, Alg, Out]
