// package neurocat
// package idag


// import shapeless.{::, HNil}
// import typeclasses._
// import neurocat.nd4j._
// import singleton.ops._
// import shapeless.labelled._


// // trait ND4J

// object ND4J {
//   type _1 = W.`1`.T

//   trait Algebra[S, Alg[out[p, a, b]] <: ND4J.Algebra[S, Alg, out], Out[p, a, b]]
//   extends DagAlgebra[S, Alg, Out]
//   with Sigmoid.Algebra[S, Alg, Out]
//   with DenseLayer.Algebra[S, Alg, Out]
//   with CostL2.Algebra[S, Alg, Out]
//   with CostL2Diff.Algebra[S, Alg, Out]
//   with CostL2DiffInvert.Algebra[S, Alg, Out]
//   with ScalarTimes.Algebra[S, Alg, Out]

//   // trait DiffAlgebra[
//   //   Alg[out[p, a, b]] <: ND4J.DiffAlgebra[Alg, out, S, K], Out[p, a, b]
//   // , S, K[alg[out[p, a, b]] <: DiffDagAlgebra[alg, out, s, K], s] <: Calculus[alg, s, K]
//   // ]
//   // extends DiffDagAlgebra[Alg, Out, S, K]
//   // with Algebra[Alg, Out]
//   // with Sigmoid.DiffAlgebra[Alg, Out, S, K]
//   // with DenseLayer.DiffAlgebra[Alg, Out, S, K]

//   trait DiffAlgebra[
//     S, Alg[out[p, a, b]] <: ND4J.DiffAlgebra[S, Alg, out], Out[p, a, b]
//   ]
//   extends DiffDagAlgebra[S, Alg, Out]
//   with Algebra[S, Alg, Out]
//   with Sigmoid.DiffAlgebra[S, Alg, Out]
//   with DenseLayer.DiffAlgebra[S, Alg, Out]



//   trait Dsl[S, Alg[out[p, a, b]] <: ND4J.Algebra[S, Alg, out]] {

//     def sigmoid[R <: XInt]: Dag[HNil, Mat[S, R x _1], Mat[S, R x _1], S, Alg] = new Sigmoid()

//     def denseLayer[Sym <: Singleton, In <: Dim2[_, _], Out <: Dim2[_, _]](
//       implicit same: Dim2SameCol[In, Out]
//     ): Dag[
//         FieldType[Sym, Mat[S, same.OutR x same.In]] :: HNil
//       , Mat[S, same.In x same.OutC]
//       , Mat[S, same.OutR x same.OutC]
//       , S, Alg
//     ] = new DenseLayer()

//   }

//   // trait DiffDsl[
//   //   Alg[out[p, a, b]] <: ND4J.DiffAlgebra[Alg, out, S, K]
//   // , S, K[alg[out[p, a, b]] <: DiffDagAlgebra[alg, out, s, K], s] <: Calculus[alg, s, K]
//   // ] {

//   //   def sigmoid[R <: XInt, C <: XInt](
//   //     implicit costA0: CostBuilder[Mat[S, R x C], Alg, S, K]
//   //   ): DiffDag[HNil, Mat[S, R x C], Mat[S, R x C], Alg, S, K] =
//   //     new Sigmoid.Diff[S, R, C, Alg, K] {
//   //       val costA = costA0
//   //       val costB = costA0
//   //     }

//   //   def denseLayer[Sym <: Singleton, InR <: XInt, OutC <: XInt, OutR <: XInt](
//   //     implicit
//   //       costIn: CostBuilder[Mat[S, InR x OutC], Alg, S, K]
//   //     , costOut: CostBuilder[Mat[S, OutR x OutC], Alg, S, K]
//   //   ): DiffDag[
//   //       FieldType[Sym, Mat[S, OutR x InR]] :: HNil
//   //     , Mat[S, InR x OutC]
//   //     , Mat[S, OutR x OutC]
//   //     , Alg
//   //     , S, K
//   //   ] = new DenseLayer.Diff[Sym, S, InR, OutR, OutC, Alg, K] {
//   //     val costA = costIn
//   //     val costB = costOut
//   //   }
//   // }



//   trait DiffDsl[
//     S, Alg[out[p, a, b]] <: ND4J.DiffAlgebra[S, Alg, out]
//   ] {

//     def sigmoid[R <: XInt, C <: XInt](
//       implicit
//         costDiffInvertRC: CostDiffInvertBuilder[Mat[S, R x C], S, Alg]
//       , costDiffRC: CostDiffBuilder[Mat[S, R x C], S, Alg]
//       // , normRC: NormBuilder[Mat[S, R x C], S, Alg]
//       , scalarTimesRC: ScalarTimesBuilder[Mat[S, R x C], S, Alg]
//     ): DiffDag[HNil, Mat[S, R x C], Mat[S, R x C], S, Alg] =
//       new Sigmoid.Diff[S, R, C, Alg] {
//         val costDiffInvert = costDiffInvertRC
//         val costDiff = costDiffRC
//         // val norm = normRC
//         val scalarTimes = scalarTimesRC
//       }

//     def denseLayer[Sym <: Singleton, InR <: XInt, OutC <: XInt, OutR <: XInt](
//       implicit
//         costIn: CostDiffInvertBuilder[Mat[S, InR x OutC], S, Alg]
//       , costOut: CostDiffBuilder[Mat[S, OutR x OutC], S, Alg]
//       // , normOut: NormBuilder[Mat[S, OutR x OutC], S, Alg]
//       , scalarTimesOut: ScalarTimesBuilder[Mat[S, OutR x OutC], S, Alg]
//     ): DiffDag[
//         FieldType[Sym, Mat[S, OutR x InR]] :: HNil
//       , Mat[S, InR x OutC]
//       , Mat[S, OutR x OutC]
//       , S, Alg
//     ] = new DenseLayer.Diff[Sym, S, InR, OutR, OutC, Alg] {
//       val costDiffInvert = costIn
//       val costDiff = costOut
//       // val norm = normOut
//       val scalarTimes = scalarTimesOut
//     }
//   }


//   // class L2[Alg[out[p, a, b]] <: DiffDagAlgebra[Alg, out, S, L2], S]() extends Calculus[Alg, S, L2]

//   // implicit def cbuilder[S, D <: Dim, Alg[out[p, a, b]] <: ND4J.DiffAlgebra[Alg, out, S, L2]] =
//   //   new CostBuilder[Mat[S, D], Alg, S, L2] {
//   //     def apply() = new CostL2()
//   //   }

//   // class L2[S, Alg[out[p, a, b]] <: DiffDagAlgebra[Alg, out]]() extends Calculus[S, Alg]

//   object L2 {
//     implicit def costBuilder[S, D <: Dim, Alg[out[p, a, b]] <: ND4J.DiffAlgebra[S, Alg, out]] =
//       new CostBuilder[Mat[S, D], S, Alg] {
//         def apply() = new CostL2()
//       }

//     implicit def costDiffBuilder[S, D <: Dim, Alg[out[p, a, b]] <: ND4J.DiffAlgebra[S, Alg, out]] =
//       new CostDiffBuilder[Mat[S, D], S, Alg] {
//         def apply() = new CostL2Diff()
//       }

//     implicit def costDiffInvertBuilder[S, D <: Dim, Alg[out[p, a, b]] <: ND4J.DiffAlgebra[S, Alg, out]] =
//       new CostDiffInvertBuilder[Mat[S, D], S, Alg] {
//         def apply() = new CostL2DiffInvert()
//       }

//     implicit def scalarTimesBuilder[S, D <: Dim, Alg[out[p, a, b]] <: ND4J.DiffAlgebra[S, Alg, out]] =
//       new ScalarTimesBuilder[Mat[S, D], S, Alg] {
//         def apply() = new ScalarTimes()
//       }
//   }
// }


// class Sigmoid[
//   S, R <: XInt, C <: XInt
// , Alg[out[p, a, b]] <: ND4J.Algebra[S, Alg, out]
// ]()
// extends Dag[HNil, Mat[S, R x C], Mat[S, R x C], S, Alg] {
//   self =>
//   def compile[Out[p, a, b]](
//     compiler: Alg[Out]
//   ): Out[HNil, Mat[S, R x C], Mat[S, R x C]] =
//     compiler.compile(self)
// }

// object Sigmoid {

//   abstract class Diff[
//     S, R <: XInt, C <: XInt
//   , Alg[out[p, a, b]] <: ND4J.DiffAlgebra[S, Alg, out]
//   ]()
//   extends Sigmoid[S, R, C, Alg] with DiffDag[HNil, Mat[S, R x C], Mat[S, R x C], S, Alg] {
//     self =>

//    def gradA = new Dag[HNil, (Mat[S, R x C], Mat[S, R x C]), Mat[S, R x C], S, Alg] {
//       def compile[Out[p, a, b]](
//         compiler: Alg[Out]
//       ): Out[HNil, (Mat[S, R x C], Mat[S, R x C]), Mat[S, R x C]] = compiler.grada(self)
//     }

//     def gradP = new Dag[HNil, (Mat[S, R x C], Mat[S, R x C]), HNil, S, Alg] {
//       def compile[Out[p, a, b]](
//         compiler: Alg[Out]
//       ): Out[HNil, (Mat[S, R x C], Mat[S, R x C]), HNil] = compiler.gradp(self)
//     }

//   }

//   trait Algebra[S, Alg[out[p, a, b]] <: ND4J.Algebra[S, Alg, out], Out[p, a, b]]
//   extends DagAlgebra[S, Alg, Out] {

//     def compile[
//       R <: XInt, C <: XInt
//     ](
//       dag: Sigmoid[S, R, C, Alg]
//     ): Out[HNil, Mat[S, R x C], Mat[S, R x C]]

//   }

//   trait DiffAlgebra[
//     S, Alg[out[p, a, b]] <: ND4J.DiffAlgebra[S, Alg, out], Out[p, a, b]
//   ]
//   extends Algebra[S, Alg, Out] {

//     def grada[
//       R <: XInt, C <: XInt
//     ](
//       dag: Sigmoid.Diff[S, R, C, Alg]
//     ): Out[HNil, (Mat[S, R x C], Mat[S, R x C]), Mat[S, R x C]]

//     def gradp[
//       R <: XInt, C <: XInt
//     ](
//       dag: Sigmoid.Diff[S, R, C, Alg]
//     ): Out[HNil, (Mat[S, R x C], Mat[S, R x C]), HNil]
//   }
// }


// class DenseLayer[
//   Sym <: Singleton, S, InR <: XInt, OutR <: XInt, OutC <: XInt
// , Alg[out[p, a, b]] <: DenseLayer.Algebra[S, Alg, out]
// ]() extends Dag[
//     FieldType[Sym, Mat[S, OutR x InR]] :: HNil
//   , Mat[S, InR x OutC]
//   , Mat[S, OutR x OutC]
//   , S, Alg
//   ] {
//     self =>
//     def compile[Out[p, a, b]](
//       compiler: Alg[Out]
//     ): Out[
//       FieldType[Sym, Mat[S, OutR x InR]] :: HNil
//     , Mat[S, InR x OutC]
//     , Mat[S, OutR x OutC]
//     ] = compiler.compile(self)
//   }

// object DenseLayer {


//   abstract class Diff[
//     Sym <: Singleton, S, InR <: XInt, OutR <: XInt, OutC <: XInt
//   , Alg[out[p, a, b]] <: ND4J.DiffAlgebra[S, Alg, out]
//   ]()
//   extends DenseLayer[Sym, S, InR, OutR, OutC, Alg] with DiffDag[
//     FieldType[Sym, Mat[S, OutR x InR]] :: HNil
//   , Mat[S, InR x OutC]
//   , Mat[S, OutR x OutC]
//   , S, Alg
//   ] {
//     self =>

//     def gradA = new Dag[FieldType[Sym, Mat[S, OutR x InR]] :: HNil, (Mat[S, InR x OutC], Mat[S, OutR x OutC]), Mat[S, InR x OutC], S, Alg] {
//         def compile[Out[p, a, b]](
//           compiler: Alg[Out]
//         ): Out[FieldType[Sym, Mat[S, OutR x InR]] :: HNil, (Mat[S, InR x OutC], Mat[S, OutR x OutC]), Mat[S, InR x OutC]] = compiler.grada(self)
//       }

//     def gradP = new Dag[FieldType[Sym, Mat[S, OutR x InR]] :: HNil, (Mat[S, InR x OutC], Mat[S, OutR x OutC]), FieldType[Sym, Mat[S, OutR x InR]] :: HNil, S, Alg] {
//       def compile[Out[p, a, b]](
//         compiler: Alg[Out]
//       ): Out[FieldType[Sym, Mat[S, OutR x InR]] :: HNil, (Mat[S, InR x OutC], Mat[S, OutR x OutC]), FieldType[Sym, Mat[S, OutR x InR]] :: HNil] = compiler.gradp(self)
//     }

//   }

//   trait Algebra[S, Alg[out[p, a, b]] <: Algebra[S, Alg, out], Out[p, a, b]]
//   extends DagAlgebra[S, Alg, Out] {

//     def compile[Sym <: Singleton, InR <: XInt, OutR <: XInt, OutC <: XInt](
//       dag: DenseLayer[Sym, S, InR, OutR, OutC, Alg]
//     ): Out[
//       FieldType[Sym, Mat[S, OutR x InR]] :: HNil
//     , Mat[S, InR x OutC]
//     , Mat[S, OutR x OutC]
//     ]

//   }


//   trait DiffAlgebra[
//     S, Alg[out[p, a, b]] <: ND4J.DiffAlgebra[S, Alg, out], Out[p, a, b]
//   ] extends Algebra[S, Alg, Out] {

//     def grada[
//       Sym <: Singleton, InR <: XInt, OutR <: XInt, OutC <: XInt
//     ](
//       dag: DenseLayer.Diff[Sym, S, InR, OutR, OutC, Alg]
//     ): Out[FieldType[Sym, Mat[S, OutR x InR]] :: HNil, (Mat[S, InR x OutC], Mat[S, OutR x OutC]), Mat[S, InR x OutC]]

//     def gradp[
//       Sym <: Singleton, InR <: XInt, OutR <: XInt, OutC <: XInt
//     ](
//       dag: DenseLayer.Diff[Sym, S, InR, OutR, OutC, Alg]
//     ): Out[FieldType[Sym, Mat[S, OutR x InR]] :: HNil, (Mat[S, InR x OutC], Mat[S, OutR x OutC]), FieldType[Sym, Mat[S, OutR x InR]] :: HNil]
//   }
// }


// class CostL2[
//   S, D <: Dim
// , Alg[out[p, a, b]] <: ND4J.Algebra[S, Alg, out]
// ] extends Dag[HNil, (Mat[S, D], Mat[S, D]), Mat[S, D], S, Alg] {
//   self =>
//   def compile[Out[p, a, b]](
//     compiler: Alg[Out]
//   ): Out[HNil, (Mat[S, D], Mat[S, D]), Mat[S, D]] =
//     compiler.compile(self)
// }

// object CostL2 {

//   trait Algebra[S, Alg[out[p, a, b]] <: ND4J.Algebra[S, Alg, out], Out[p, a, b]]
//   extends DagAlgebra[S, Alg, Out] {

//     def compile[D <: Dim](dag: CostL2[S, D, Alg]): Out[HNil, (Mat[S, D], Mat[S, D]), Mat[S, D]]

//   }

// }


// class CostL2Diff[
//   S, D <: Dim
// , Alg[out[p, a, b]] <: ND4J.Algebra[S, Alg, out]
// ] extends Dag[HNil, (Mat[S, D], Mat[S, D]), Mat[S, D], S, Alg] {
//   self =>
//   def compile[Out[p, a, b]](
//     compiler: Alg[Out]
//   ): Out[HNil, (Mat[S, D], Mat[S, D]), Mat[S, D]] =
//     compiler.compile(self)
// }

// object CostL2Diff {

//   trait Algebra[S, Alg[out[p, a, b]] <: ND4J.Algebra[S, Alg, out], Out[p, a, b]]
//   extends DagAlgebra[S, Alg, Out] {

//     def compile[D <: Dim](dag: CostL2Diff[S, D, Alg]): Out[HNil, (Mat[S, D], Mat[S, D]), Mat[S, D]]

//   }

// }

// class CostL2DiffInvert[
//   S, D <: Dim
// , Alg[out[p, a, b]] <: ND4J.Algebra[S, Alg, out]
// ] extends Dag[HNil, (Mat[S, D], Mat[S, D]), Mat[S, D], S, Alg] {
//   self =>
//   def compile[Out[p, a, b]](
//     compiler: Alg[Out]
//   ): Out[HNil, (Mat[S, D], Mat[S, D]), Mat[S, D]] =
//     compiler.compile(self)
// }

// object CostL2DiffInvert {

//   trait Algebra[S, Alg[out[p, a, b]] <: ND4J.Algebra[S, Alg, out], Out[p, a, b]]
//   extends DagAlgebra[S, Alg, Out] {

//     def compile[D <: Dim](dag: CostL2DiffInvert[S, D, Alg]): Out[HNil, (Mat[S, D], Mat[S, D]), Mat[S, D]]

//   }

// }


// class ScalarTimes[
//   S, D <: Dim
// , Alg[out[p, a, b]] <: ND4J.Algebra[S, Alg, out]
// ] extends Dag[HNil, (S, Mat[S, D]), Mat[S, D], S, Alg] {
//   self =>
//   def compile[Out[p, a, b]](
//     compiler: Alg[Out]
//   ): Out[HNil, (S, Mat[S, D]), Mat[S, D]] =
//     compiler.compile(self)
// }

// object ScalarTimes {

//   trait Algebra[S, Alg[out[p, a, b]] <: ND4J.Algebra[S, Alg, out], Out[p, a, b]]
//   extends DagAlgebra[S, Alg, Out] {

//     def compile[D <: Dim](dag: ScalarTimes[S, D, Alg]): Out[HNil, (S, Mat[S, D]), Mat[S, D]]

//   }

// }



