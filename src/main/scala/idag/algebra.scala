package neurocat
package idag



trait DagAlgebra[
  S, Alg[out[p, a, b]] <: DagAlgebra[S, Alg, out], Out[p, a, b]
]
extends Id.Algebra[S, Alg, Out]
with Compose.Algebra[S, Alg, Out]
with Prod.Algebra[S, Alg, Out]
with Apply.Algebra[S, Alg, Out]
with Fst.Algebra[S, Alg, Out]
with Join.Algebra[S, Alg, Out]
with Snd.Algebra[S, Alg, Out]
with Const.Algebra[S, Alg, Out]
with Split.Algebra[S, Alg, Out]
with SlideR.Algebra[S, Alg, Out]
with SlideL.Algebra[S, Alg, Out]


trait DiffDagAlgebra[
  S, Alg[out[p, a, b]] <: DiffDagAlgebra[S, Alg, out], Out[p, a, b]
]
extends DagAlgebra[S, Alg, Out]
with Id.DiffAlgebra[S, Alg, Out]
with Compose.DiffAlgebra[S, Alg, Out]
with Prod.DiffAlgebra[S, Alg, Out]
with Fst.DiffAlgebra[S, Alg, Out] 
with Snd.DiffAlgebra[S, Alg, Out] 
with Join.DiffAlgebra[S, Alg, Out]
with Const.DiffAlgebra[S, Alg, Out]
with Split.DiffAlgebra[S, Alg, Out]
with SlideR.DiffAlgebra[S, Alg, Out]
with SlideL.DiffAlgebra[S, Alg, Out]
