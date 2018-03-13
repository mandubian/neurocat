package neurocat
package test

import idag._
import idag.nd4j._
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


//////////////////////////////////////////////////////////////////
// ID
trait IdDiffStringCompiler extends Id.DiffAlgebra[Double, ND4JAlgebra, Lambda[(p, a, b) => String]] {
  def compile[A](
    dag: idag.Id[A, Double, ND4JAlgebra]
  ) = "Id"

  def grada[A](
    dag: idag.Id.Diff[A, Double, ND4JAlgebra]
  ) = "Id:GradA"

  def gradp[A](
    dag: idag.Id.Diff[A, Double, ND4JAlgebra]
  ) = "Id:GradP"
}

//////////////////////////////////////////////////////////////////
// CONST
trait ConstDiffStringCompiler extends Const.DiffAlgebra[Double, ND4JAlgebra, Lambda[(p, a, b) => String]] {

  def compile[A](
    dag: idag.Const[A, Double, ND4JAlgebra]
  ) = s"Const(${dag.value()})"

  def grada[A](
    dag: Const.Diff[A, Double, ND4JAlgebra]
  ) = s"Const(${dag.value()}):GradA"

  def gradp[A](
    dag: Const.Diff[A, Double, ND4JAlgebra]
  ) = s"Const(${dag.value()}):GradP"
}

//////////////////////////////////////////////////////////////////
// FST
trait FstDiffStringCompiler extends Fst.DiffAlgebra[Double, ND4JAlgebra, Lambda[(p, a, b) => String]] {

  def compile[A, B](
    dag: Fst[A, B, Double, ND4JAlgebra]
  ) = "Fst"

  def grada[A, B : Zeroed](
    dag: Fst.Diff[A, B, Double, ND4JAlgebra]
  ) = "Fst:GradA"

  def gradp[A, B](
    dag: Fst.Diff[A, B, Double, ND4JAlgebra]
  ) = "Fst:GradP"
}

//////////////////////////////////////////////////////////////////
// SND
trait SndDiffStringCompiler extends Snd.DiffAlgebra[Double, ND4JAlgebra, Lambda[(p, a, b) => String]] {

  def compile[A, B](
    dag: Snd[A, B, Double, ND4JAlgebra]
  ) = "Snd"

  def grada[A : Zeroed, B](
    dag: Snd.Diff[A, B, Double, ND4JAlgebra]
  ) = "Snd:GradA"

  def gradp[A, B](
    dag: Snd.Diff[A, B, Double, ND4JAlgebra]
  ) = "Snd:GradP"
}

//////////////////////////////////////////////////////////////////
// SPLIT
trait SplitDiffStringCompiler extends Split.DiffAlgebra[Double, ND4JAlgebra, Lambda[(p, a, b) => String]] {

  def compile[A, B, C](
    dag: Split[A, B, C, Double, ND4JAlgebra]
  ) = "Split"

  def grada[A, B, C](
    dag: Split.Diff[A, B, C, Double, ND4JAlgebra]
  ) = "Split:GradA"

  def gradp[A, B, C](
    dag: Split.Diff[A, B, C, Double, ND4JAlgebra]
  ) = "Split:GradP"
}

//////////////////////////////////////////////////////////////////
// JOIN
trait JoinDiffStringCompiler extends Join.DiffAlgebra[Double, ND4JAlgebra, Lambda[(p, a, b) => String]] {

  def compile[A, B, C](
    dag: Join[A, B, C, Double, ND4JAlgebra]
  ) = "Join"

  def grada[A, B, C](
    dag: Join.Diff[A, B, C, Double, ND4JAlgebra]
  ) = "Join:GradA"

  def gradp[A, B, C](
    dag: Join.Diff[A, B, C, Double, ND4JAlgebra]
  ) = "Join:GradP"
}

//////////////////////////////////////////////////////////////////
// SLIDEL
trait SlideLDiffStringCompiler extends SlideL.DiffAlgebra[Double, ND4JAlgebra, Lambda[(p, a, b) => String]] {
  self : ND4JAlgebra[Lambda[(p, a, b) => String]] =>
  def compile[P, A, B](
    dag: SlideL[P, A, B, Double, ND4JAlgebra]
  ) = s"SlideL(${dag.dag.compile[Lambda[(p, a, b) => String]](self)})"

  def grada[P, A, B](
    dag: SlideL.Diff[P, A, B, Double, ND4JAlgebra]
  ) = s"SlideL:GradA(${dag.dag.gradA.compile[Lambda[(p, a, b) => String]](self)})"

  def gradp[P, A, B](
    dag: SlideL.Diff[P, A, B, Double, ND4JAlgebra]
  ) = s"SlideL:GradP(${dag.dag.gradP.compile[Lambda[(p, a, b) => String]](self)})"

}

//////////////////////////////////////////////////////////////////
// SLIDER
trait SlideRDiffStringCompiler extends SlideR.DiffAlgebra[Double, ND4JAlgebra, Lambda[(p, a, b) => String]] {
  self : ND4JAlgebra[Lambda[(p, a, b) => String]] =>

  def compile[P, A, B](
    dag: SlideR[P, A, B, Double, ND4JAlgebra]
  ) = s"SlideR(${dag.dag.compile[Lambda[(p, a, b) => String]](self)})"

  def grada[P, A, B](
    dag: SlideR.Diff[P, A, B, Double, ND4JAlgebra]
  ) = s"SlideR:GradA(${dag.dag.gradA.compile[Lambda[(p, a, b) => String]](self)})"

  def gradp[P, A, B](
    dag: SlideR.Diff[P, A, B, Double, ND4JAlgebra]
  ) = s"SlideR:GradP(${dag.dag.gradP.compile[Lambda[(p, a, b) => String]](self)})"
}


//////////////////////////////////////////////////////////////////
// PROD
trait ProdDiffStringCompiler extends Prod.DiffAlgebra[Double, ND4JAlgebra, Lambda[(p, a, b) => String]] {
  self : ND4JAlgebra[Lambda[(p, a, b) => String]] =>
  
  def compile[
    P, PA, PB
  , Q, QB
  , PQ
  ](
    dag: Prod[P, PA, PB, Q, QB, PQ, Double, ND4JAlgebra]
  ) =
    s"${dag.f.compile[Lambda[(p, a, b) => String]](self)} ** ${dag.g.compile[Lambda[(p, a, b) => String]](self)}"

  def grada[
    P, PA : AdditiveSemigroup, PB
  , Q, QB
  , PQ
  ](
    dag: Prod.Diff[P, PA, PB, Q, QB, PQ, Double, ND4JAlgebra]
  ) = 
    s"${dag.f.gradA.compile[Lambda[(p, a, b) => String]](self)} ** ${dag.g.gradA.compile[Lambda[(p, a, b) => String]](self)}"

  def gradp[
    P, PA : AdditiveSemigroup, PB
  , Q, QB
  , PQ
  ](
    dag: Prod.Diff[P, PA, PB, Q, QB, PQ, Double, ND4JAlgebra]
  ) =
    s"${dag.f.gradP.compile[Lambda[(p, a, b) => String]](self)} ** ${dag.g.gradP.compile[Lambda[(p, a, b) => String]](self)}"

}


//////////////////////////////////////////////////////////////////
// COMPOSE
trait ComposeDiffStringCompiler extends Prod.DiffAlgebra[Double, ND4JAlgebra, Lambda[(p, a, b) => String]] {
  self : ND4JAlgebra[Lambda[(p, a, b) => String]] =>

  def compile[
    P, PA, PB
  , Q, QC
  , PQ
  ](
    dag: Compose[P, PA, PB, Q, QC, PQ, Double, ND4JAlgebra]
  ) =
    s"${dag.f.compile[Lambda[(p, a, b) => String]](self)} >>> ${dag.g.compile[Lambda[(p, a, b) => String]](self)}"


  def grada[
    P, PA, PB
  , Q, QC
  , PQ
  ](
    dag: Compose.Diff[P, PA, PB, Q, QC, PQ, Double, ND4JAlgebra]
  ) =
    s"${dag.f.gradA.compile[Lambda[(p, a, b) => String]](self)} >>> ${dag.g.gradA.compile[Lambda[(p, a, b) => String]](self)}"


  def gradp[
    P, PA, PB
  , Q, QC
  , PQ
  ](
    dag: Compose.Diff[P, PA, PB, Q, QC, PQ, Double, ND4JAlgebra]
  ) =
    s"${dag.f.gradP.compile[Lambda[(p, a, b) => String]](self)} >>> ${dag.g.gradP.compile[Lambda[(p, a, b) => String]](self)}"

}


//////////////////////////////////////////////////////////////////
// Apply
trait ApplyStringCompiler extends Apply.Algebra[Double, ND4JAlgebra, Lambda[(p, a, b) => String]] {
  self : ND4JAlgebra[Lambda[(p, a, b) => String]] =>

  def compile[P, A, B](
    dag: Apply[P, A, B, Double, ND4JAlgebra]
  ) =
    s"Apply"
}



//////////////////////////////////////////////////////////////////
// ND4J

trait ND4JDiffStringCompiler extends nd4j.DiffAlgebra[Double, ND4JAlgebra, Lambda[(p, a, b) => String]] {
  self : ND4JAlgebra[Lambda[(p, a, b) => String]] =>


//////////////////////////////////////////////////////////////////
// SIGMOID
  def compile[
    R <: XInt, C <: XInt
  ](
    dag: Sigmoid[Double, R, C, ND4JAlgebra]
  ) = s"Sigmoid[Double, R, C]"

  def grada[
    R <: XInt, C <: XInt
  ](
    dag: Sigmoid.Diff[Double, R, C, ND4JAlgebra]
  ) = s"Sigmoid:GradA[Double, R, C]"

  def gradp[
    R <: XInt, C <: XInt
  ](
    dag: Sigmoid.Diff[Double, R, C, ND4JAlgebra]
  ) = s"Sigmoid:GradP[Double, R, C]"

//////////////////////////////////////////////////////////////////
// WeightMat

  def compile[Sym <: Singleton, InR <: XInt, OutR <: XInt, OutC <: XInt](
    dag: WeightMat[Sym, Double, InR, OutR, OutC, ND4JAlgebra]
  ) = s"WeightMat[InR x OutC, OutR x OutC]"

  def grada[Sym <: Singleton, InR <: XInt, OutR <: XInt, OutC <: XInt](
    dag: WeightMat.Diff[Sym, Double, InR, OutR, OutC, ND4JAlgebra]
  ) = s"WeightMat:GradA[InR x OutC, OutR x OutC]"

  def gradp[Sym <: Singleton, InR <: XInt, OutR <: XInt, OutC <: XInt](
    dag: WeightMat.Diff[Sym, Double, InR, OutR, OutC, ND4JAlgebra]
  ) = s"WeightMat:GradP[InR x OutC, OutR x OutC]"


//////////////////////////////////////////////////////////////////
// Dense

  def compile[InR <: XInt, OutR <: XInt, OutC <: XInt](
    dag: Dense[Double, InR, OutR, OutC, ND4JAlgebra]
  ) = s"Dense[InR x OutC, OutR x OutC]"

  def grada[InR <: XInt, OutR <: XInt, OutC <: XInt](
    dag: Dense.Diff[Double, InR, OutR, OutC, ND4JAlgebra]
  ) = s"Dense:GradA[InR x OutC, OutR x OutC]"

  def gradp[InR <: XInt, OutR <: XInt, OutC <: XInt](
    dag: Dense.Diff[Double, InR, OutR, OutC, ND4JAlgebra]
  ) = s"Dense:GradP[InR x OutC, OutR x OutC]"


//////////////////////////////////////////////////////////////////
// Relu
  def compile[R <: XInt, C <: XInt](
    dag: Relu[Double, R, C, ND4JAlgebra]
  ) = s"Relu[R x C]"

  def gradp[R <: XInt, C <: XInt](
    dag: Relu.Diff[Double, R, C, ND4JAlgebra]
  ) = s"Relu:GradP[R x C]"

  def grada[R <: XInt : SafeInt, C <: XInt : SafeInt](
    dag: Relu.Diff[Double, R, C, ND4JAlgebra]
  ) = s"Relu:GradA[R x C]"

//////////////////////////////////////////////////////////////////
// BiasMat
  def compile[Sym <: Singleton, R <: XInt, C <: XInt](
    dag: BiasMat[Sym, Double, R, C, ND4JAlgebra]
  ) = s"BiasMat[R x C]"

  def gradp[Sym <: Singleton, R <: XInt, C <: XInt](
    dag: BiasMat.Diff[Sym, Double, R, C, ND4JAlgebra]
  ) = s"BiasMat:GradP[R x C]"


  def grada[Sym <: Singleton, R <: XInt : SafeInt, C <: XInt : SafeInt](
    dag: BiasMat.Diff[Sym, Double, R, C, ND4JAlgebra]
  ) = s"BiasMat:GradA[R x C]"

  val fm = new org.nd4j.linalg.string.NDArrayStrings(8)

//////////////////////////////////////////////////////////////////
// L2

  def compile[D <: Dim](dag: CostL2[Double, D, ND4JAlgebra]) = 
    s"CostL2[Double, D]"

  def compile[D <: Dim](dag: CostL2Diff[Double, D, ND4JAlgebra]) = 
    s"CostL2Diff[Double, D]"


  def compile[D <: Dim](dag: CostL2DiffInvert[Double, D, ND4JAlgebra]) = 
    s"CostL2DiffInvert[Double, D]"


//////////////////////////////////////////////////////////////////
// OPS

  def compile[D <: Dim](dag: ScalarTimes[Double, D, ND4JAlgebra]) = 
    s"ScalarTimes[Double, D]"

  def compile[D <: Dim](dag: Minus[D, Double, ND4JAlgebra]) = 
    s"Minus[Double, D]"


  def compile[A, B](dag: Func[A, B, Double, ND4JAlgebra]) = 
    s"Func[Double, D]"

}


object ND4JStringCompiler
extends ND4JAlgebra[Lambda[(p, a, b) => String]]
with IdDiffStringCompiler
with ProdDiffStringCompiler
with ComposeDiffStringCompiler
with ConstDiffStringCompiler
with FstDiffStringCompiler
with SndDiffStringCompiler
with SplitDiffStringCompiler
with JoinDiffStringCompiler
with ApplyStringCompiler
with SlideRDiffStringCompiler
with SlideLDiffStringCompiler
with ND4JDiffStringCompiler
