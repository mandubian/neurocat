package neurocat
package idag
package nd4j


import shapeless.{::, HNil}
import typeclasses._
import neurocat.nd4j._



trait Dsl[S, Alg[out[p, a, b]] <: nd4j.Algebra[S, Alg, out]]
extends Sigmoid.Dsl[S, Alg]
with WeightMat.Dsl[S, Alg]
with BiasMat.Dsl[S, Alg]
with Func.Dsl[S, Alg]


trait DiffDsl[S, Alg[out[p, a, b]] <: nd4j.DiffAlgebra[S, Alg, out]]
extends Sigmoid.DiffDsl[S, Alg]
with WeightMat.DiffDsl[S, Alg]
with BiasMat.DiffDsl[S, Alg]
