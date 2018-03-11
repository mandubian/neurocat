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


// trait ND4J

trait ND4JAlgebra[Out[p, a, b]]
  extends DiffDagAlgebra[Double, ND4JAlgebra, Out]
  with nd4j.DiffAlgebra[Double, ND4JAlgebra, Out]

//////////////////////////////////////////////////////////////////
// ID
trait IdDiffCompiler extends Id.DiffAlgebra[Double, ND4JAlgebra, ParametrisedFunction] {
  // self : ND4JAlgebra[ParametrisedFunction] =>
  def compile[A](
    dag: idag.Id[A, Double, ND4JAlgebra]
  ) = new ParametrisedFunction[HNil, A, A] {
    def apply(p: HNil, a: A): A = a
  }

  def grada[A](
    dag: idag.Id.Diff[A, Double, ND4JAlgebra]
  ) = new ParametrisedFunction[HNil, (A, A), A] {
    def apply(p: HNil, a: (A, A)): A = a._2
  }

  def gradp[A](
    dag: idag.Id.Diff[A, Double, ND4JAlgebra]
  ) = new ParametrisedFunction[HNil, (A, A), HNil] {
    def apply(p: HNil, a: (A, A)): HNil = HNil
  }
}

//////////////////////////////////////////////////////////////////
// CONST
trait ConstDiffCompiler extends Const.DiffAlgebra[Double, ND4JAlgebra, ParametrisedFunction] {
  self : ND4JAlgebra[ParametrisedFunction] =>

  def compile[A](
    dag: idag.Const[A, Double, ND4JAlgebra]
  ) = new ParametrisedFunction[HNil, Unit, A] {
    def apply(p: HNil, a: Unit): A = dag.value()
  }

  def grada[A](
    dag: Const.Diff[A, Double, ND4JAlgebra]
  ) = new ParametrisedFunction[HNil, (Unit, A), Unit] {
    def apply(p: HNil, a: (Unit, A)): Unit = ()
  }

  def gradp[A](
    dag: Const.Diff[A, Double, ND4JAlgebra]
  ) = new ParametrisedFunction[HNil, (Unit, A), HNil] {
    def apply(p: HNil, a: (Unit, A)): HNil = HNil
  }
}

//////////////////////////////////////////////////////////////////
// FST
trait FstDiffCompiler extends Fst.DiffAlgebra[Double, ND4JAlgebra, ParametrisedFunction] {
  self : ND4JAlgebra[ParametrisedFunction] =>

  def compile[A, B](
    dag: Fst[A, B, Double, ND4JAlgebra]
  ) = new ParametrisedFunction[HNil, (A, B), A] {
    def apply(p: HNil, a: (A, B)): A = a._1
  }

  def grada[A, B : Zeroed](
    dag: Fst.Diff[A, B, Double, ND4JAlgebra]
  ) = new ParametrisedFunction[HNil, ((A, B), A), (A, B)] {
    def apply(p: HNil, a: ((A, B), A)): (A, B) = (a._2, Zeroed[B].zero)
  }

  def gradp[A, B](
    dag: Fst.Diff[A, B, Double, ND4JAlgebra]
  ) = new ParametrisedFunction[HNil, ((A, B), A), HNil] {
    def apply(p: HNil, a: ((A, B), A)): HNil = HNil
  }
}


//////////////////////////////////////////////////////////////////
// SND
trait SndDiffCompiler extends Snd.DiffAlgebra[Double, ND4JAlgebra, ParametrisedFunction] {
  self : ND4JAlgebra[ParametrisedFunction] =>

  def compile[A, B](
    dag: Snd[A, B, Double, ND4JAlgebra]
  ) = new ParametrisedFunction[HNil, (A, B), B] {
    def apply(p: HNil, a: (A, B)): B = a._2
  }

  def grada[A : Zeroed, B](
    dag: Snd.Diff[A, B, Double, ND4JAlgebra]
  ) = new ParametrisedFunction[HNil, ((A, B), B), (A, B)] {
    def apply(p: HNil, a: ((A, B), B)): (A, B) = (Zeroed[A].zero, a._2)
  }

  def gradp[A, B](
    dag: Snd.Diff[A, B, Double, ND4JAlgebra]
  ) = new ParametrisedFunction[HNil, ((A, B), B), HNil] {
    def apply(p: HNil, a: ((A, B), B)): HNil = HNil
  }
}    

//////////////////////////////////////////////////////////////////
// SPLIT
trait SplitDiffCompiler extends Split.DiffAlgebra[Double, ND4JAlgebra, ParametrisedFunction] {
  self : ND4JAlgebra[ParametrisedFunction] =>

  def compile[A, B, C](
    dag: Split[A, B, C, Double, ND4JAlgebra]
  ) = new ParametrisedFunction[HNil, A, (B, C)] {
    def apply(p: HNil, a: A): (B, C) = (dag.merger.left(a), dag.merger.right(a))
  }

  def grada[A, B, C](
    dag: Split.Diff[A, B, C, Double, ND4JAlgebra]
  ) = new ParametrisedFunction[HNil, (A, (B, C)), A] {
    def apply(p: HNil, a: (A, (B, C))): A = dag.merger(a._2._1, a._2._2)
  }

  def gradp[A, B, C](
    dag: Split.Diff[A, B, C, Double, ND4JAlgebra]
  ) = new ParametrisedFunction[HNil, (A, (B, C)), HNil] {
    def apply(p: HNil, a: (A, (B, C))): HNil = HNil
  }
}

//////////////////////////////////////////////////////////////////
// JOIN
trait JoinDiffCompiler extends Join.DiffAlgebra[Double, ND4JAlgebra, ParametrisedFunction] {
  self : ND4JAlgebra[ParametrisedFunction] =>

  def compile[A, B, C](
    dag: Join[A, B, C, Double, ND4JAlgebra]
  ) = new ParametrisedFunction[HNil, (A, B), C] {
    def apply(p: HNil, a: (A, B)): C = dag.merger(a._1, a._2)
  }

  def grada[A, B, C](
    dag: Join.Diff[A, B, C, Double, ND4JAlgebra]
  ) = new ParametrisedFunction[HNil, ((A, B), C), (A, B)] {
    def apply(p: HNil, a: ((A, B), C)): (A, B) = (dag.merger.left(a._2), dag.merger.right(a._2))
  }

  def gradp[A, B, C](
    dag: Join.Diff[A, B, C, Double, ND4JAlgebra]
  ) = new ParametrisedFunction[HNil, ((A, B), C), HNil] {
    def apply(p: HNil, a: ((A, B), C)): HNil = HNil
  }
}

//////////////////////////////////////////////////////////////////
// SLIDEL
trait SlideLDiffCompiler extends SlideL.DiffAlgebra[Double, ND4JAlgebra, ParametrisedFunction] {
  self : ND4JAlgebra[ParametrisedFunction] =>

  def compile[P, A, B](
    dag: SlideL[P, A, B, Double, ND4JAlgebra]
  ) = new ParametrisedFunction[P, A, B] {
    def apply(p: P, a: A): B = dag.dag.compile(self)(HNil, (p, a))
  }

  def grada[P, A, B](
    dag: SlideL.Diff[P, A, B, Double, ND4JAlgebra]
  ) = new ParametrisedFunction[P, (A, B), A] {
    def apply(p: P, a: (A, B)): A = dag.dag.gradA.compile(self)(HNil, ((p, a._1), a._2))._2
  }

  def gradp[P, A, B](
    dag: SlideL.Diff[P, A, B, Double, ND4JAlgebra]
  ) = new ParametrisedFunction[P, (A, B), P] {
    def apply(p: P, a: (A, B)): P = dag.dag.gradA.compile(self)(HNil, ((p, a._1), a._2))._1
  }

}


//////////////////////////////////////////////////////////////////
// SLIDER
trait SlideRDiffCompiler extends SlideR.DiffAlgebra[Double, ND4JAlgebra, ParametrisedFunction] {
  self : ND4JAlgebra[ParametrisedFunction] =>

  def compile[P, A, B](
    dag: SlideR[P, A, B, Double, ND4JAlgebra]
  ) = new ParametrisedFunction[HNil, (P, A), B] {
    def apply(p: HNil, pa: (P, A)): B = dag.dag.compile(self)(pa._1, pa._2)
  }

  def grada[P, A, B](
    dag: SlideR.Diff[P, A, B, Double, ND4JAlgebra]
  ) = new ParametrisedFunction[HNil, ((P, A), B), (P, A)] {
    def apply(p: HNil, pab: ((P, A), B)): (P, A) = (pab._1._1, dag.dag.gradA.compile(self)(pab._1._1, (pab._1._2, pab._2)))
  }

  def gradp[P, A, B](
    dag: SlideR.Diff[P, A, B, Double, ND4JAlgebra]
  ) = new ParametrisedFunction[HNil, ((P, A), B), HNil] {
    def apply(p: HNil, a: ((P, A), B)): HNil = HNil
  }
}


//////////////////////////////////////////////////////////////////
// PROD
trait ProdDiffCompiler extends Prod.DiffAlgebra[Double, ND4JAlgebra, ParametrisedFunction] {
  self : ND4JAlgebra[ParametrisedFunction] =>
  
  def compile[
    P, PA, PB
  , Q, QB
  , PQ
  ](
    dag: Prod[P, PA, PB, Q, QB, PQ, Double, ND4JAlgebra]
  ): ParametrisedFunction[PQ, PA, (PB, QB)] =
    new ParametrisedFunction[PQ, PA, (PB, QB)] {
      val pf = dag.f.compile(self)
      val pg = dag.g.compile(self)

      def apply(pq: PQ, ab: PA): (PB, QB) = {
        ( pf(dag.merger.left(pq), ab)
        , pg(dag.merger.right(pq), ab)
        )
      }
    }

  def grada[
    P, PA : AdditiveSemigroup, PB
  , Q, QB
  , PQ
  ](
    dag: Prod.Diff[P, PA, PB, Q, QB, PQ, Double, ND4JAlgebra]
  ): ParametrisedFunction[PQ, (PA, (PB, QB)), PA] = 
    new ParametrisedFunction[PQ, (PA, (PB, QB)), PA] {
      val pf = dag.f.gradA.compile(self)
      val pg = dag.g.gradA.compile(self)

      def apply(pq: PQ, ab: (PA, (PB, QB))): PA = {
        AdditiveSemigroup[PA].plus(
          pf(dag.merger.left(pq), (ab._1, ab._2._1))
        , pg(dag.merger.right(pq), (ab._1, ab._2._2))
      )
      }
    }

  def gradp[
    P, PA : AdditiveSemigroup, PB
  , Q, QB
  , PQ
  ](
    dag: Prod.Diff[P, PA, PB, Q, QB, PQ, Double, ND4JAlgebra]
  ): ParametrisedFunction[PQ, (PA, (PB, QB)), PQ] =
    new ParametrisedFunction[PQ, (PA, (PB, QB)), PQ] {
      val pf = dag.f.gradP.compile(self)
      val pg = dag.g.gradP.compile(self)

      def apply(pq: PQ, ab: (PA, (PB, QB))): PQ = {
        dag.merger(
          pf(dag.merger.left(pq), (ab._1, ab._2._1))
        , pg(dag.merger.right(pq), (ab._1, ab._2._2))
        )
      }
    }
}

//////////////////////////////////////////////////////////////////
// COMPOSE
trait ComposeDiffCompiler extends Prod.DiffAlgebra[Double, ND4JAlgebra, ParametrisedFunction] {
  self : ND4JAlgebra[ParametrisedFunction] =>

  def compile[
    P, PA, PB
  , Q, QC
  , PQ
  ](
    dag: Compose[P, PA, PB, Q, QC, PQ, Double, ND4JAlgebra]
  ): ParametrisedFunction[PQ, PA, QC] =
    new ParametrisedFunction[PQ, PA, QC] {
      val pg = dag.g.compile(self)
      val pf = dag.f.compile(self)

      def apply(pq: PQ, ab: PA): QC = {
        pg(dag.merger.right(pq), pf(dag.merger.left(pq), ab))
      }
    }

  def grada[
    P, PA, PB
  , Q, QC
  , PQ
  ](
    dag: Compose.Diff[P, PA, PB, Q, QC, PQ, Double, ND4JAlgebra]
  ): ParametrisedFunction[PQ, (PA, QC), PA] =
    new ParametrisedFunction[PQ, (PA, QC), PA] {
      val pggrad = dag.g.gradA.compile(self)
      val pf = dag.f.compile(self)
      val pfgrad = dag.f.gradA.compile(self)

      def apply(pq: PQ, ab: (PA, QC)): PA = {
        val pb = pf(dag.merger.left(pq), ab._1)
        val pb2 = pggrad(dag.merger.right(pq), (pb, ab._2))
        pfgrad(dag.merger.left(pq), (ab._1, pb2))
      }
    }

  def gradp[
    P, PA, PB
  , Q, QC
  , PQ
  ](
    dag: Compose.Diff[P, PA, PB, Q, QC, PQ, Double, ND4JAlgebra]
  ): ParametrisedFunction[PQ, (PA, QC), PQ] =
    new ParametrisedFunction[PQ, (PA, QC), PQ] {
      val pggrad = dag.g.gradP.compile(self)
      val pf = dag.f.compile(self)
      val pfgrad = dag.f.gradP.compile(self)

      def apply(pq: PQ, ab: (PA, QC)): PQ = {
        val p = dag.merger.left(pq)
        val q = dag.merger.right(pq)
        val (pa, qc) = ab
        val pb = pf(p, pa)
        val q2 = pggrad(q, (pb, qc))
        val p2 = pfgrad(p, (ab._1, pb))
        dag.merger(p2, q2)
      }
    }
}

//////////////////////////////////////////////////////////////////
// Apply
trait ApplyCompiler extends Apply.Algebra[Double, ND4JAlgebra, ParametrisedFunction] {
  self : ND4JAlgebra[ParametrisedFunction] =>

  def compile[P, A, B](
    dag: Apply[P, A, B, Double, ND4JAlgebra]
  ) = new ParametrisedFunction[P, (Dag[P, A, B, Double, ND4JAlgebra], A), B] {
    def apply(p: P, a: (Dag[P, A, B, Double, ND4JAlgebra], A)): B = a._1.compile(self)(p, a._2)
  }
}


//////////////////////////////////////////////////////////////////
// ND4J

trait ND4JDiffCompiler extends nd4j.DiffAlgebra[Double, ND4JAlgebra, ParametrisedFunction] {
  self : ND4JAlgebra[ParametrisedFunction] =>


//////////////////////////////////////////////////////////////////
// SIGMOID
  def compile[
    R <: XInt, C <: XInt
  ](
    dag: Sigmoid[Double, R, C, ND4JAlgebra]
  ): ParametrisedFunction[HNil, Mat[Double, R x C], Mat[Double, R x C]] = new ParametrisedFunction[HNil, Mat[Double, R x C], Mat[Double, R x C]] {
    def apply(p: HNil, a: Mat[Double, R x C]): Mat[Double, R x C] = new Mat[Double, R x C](
      Nd4j.getExecutioner().execAndReturn(new org.nd4j.linalg.api.ops.impl.transforms.Sigmoid(a.value))
    )
  }

  def grada[
    R <: XInt, C <: XInt
  ](
    dag: Sigmoid.Diff[Double, R, C, ND4JAlgebra]
  ) = new ParametrisedFunction[HNil, (Mat[Double, R x C], Mat[Double, R x C]), Mat[Double, R x C]] {
    def apply(p: HNil, a: (Mat[Double, R x C], Mat[Double, R x C])): Mat[Double, R x C] = {
      // hadamard product
      val da = Nd4j.getExecutioner().execAndReturn(
        new org.nd4j.linalg.api.ops.impl.transforms.SigmoidDerivative(a._1.value)
      )

      new Mat[Double, R x C](da.mul(a._2.value))
    }
  }

  def gradp[
    R <: XInt, C <: XInt
  ](
    dag: Sigmoid.Diff[Double, R, C, ND4JAlgebra]
  ) = new ParametrisedFunction[HNil, (Mat[Double, R x C], Mat[Double, R x C]), HNil] {
    def apply(p: HNil, a: (Mat[Double, R x C], Mat[Double, R x C])): HNil = HNil
  }

//////////////////////////////////////////////////////////////////
// WeightMat

  def compile[Sym <: Singleton, InR <: XInt, OutR <: XInt, OutC <: XInt](
    dag: WeightMat[Sym, Double, InR, OutR, OutC, ND4JAlgebra]
  ): ParametrisedFunction[
    FieldType[Sym, Mat[Double, OutR x InR]] :: HNil
  , Mat[Double, InR x OutC]
  , Mat[Double, OutR x OutC]
  ] = new ParametrisedFunction[
    FieldType[Sym, Mat[Double, OutR x InR]] :: HNil
  , Mat[Double, InR x OutC]
  , Mat[Double, OutR x OutC]
  ] {
    def apply(p: FieldType[Sym, Mat[Double, OutR x InR]] :: HNil, a: Mat[Double, InR x OutC]): Mat[Double, OutR x OutC] = {
      new Mat[Double, OutR x OutC](p.head.value.mmul(a.value))
    }
  }

  def grada[Sym <: Singleton, InR <: XInt, OutR <: XInt, OutC <: XInt](
    dag: WeightMat.Diff[Sym, Double, InR, OutR, OutC, ND4JAlgebra]
  ) = new ParametrisedFunction[
    FieldType[Sym, Mat[Double, OutR x InR]] :: HNil
  , (Mat[Double, InR x OutC], Mat[Double, OutR x OutC])
  , Mat[Double, InR x OutC]
  ] {
    def apply(
      p: FieldType[Sym, Mat[Double, OutR x InR]] :: HNil
    , a: (Mat[Double, InR x OutC], Mat[Double, OutR x OutC])
    ): Mat[Double, InR x OutC] = {
      new Mat[Double, InR x OutC](p.head.value.transpose().mmul(a._2.value))
    }
  }

  def gradp[Sym <: Singleton, InR <: XInt, OutR <: XInt, OutC <: XInt](
    dag: WeightMat.Diff[Sym, Double, InR, OutR, OutC, ND4JAlgebra]
  ) = new ParametrisedFunction[
    FieldType[Sym, Mat[Double, OutR x InR]] :: HNil
  , (Mat[Double, InR x OutC], Mat[Double, OutR x OutC])
  , FieldType[Sym, Mat[Double, OutR x InR]] :: HNil
  ] {
    def apply(
      p: FieldType[Sym, Mat[Double, OutR x InR]] :: HNil
    , a: (Mat[Double, InR x OutC], Mat[Double, OutR x OutC])
    ): FieldType[Sym, Mat[Double, OutR x InR]] :: HNil = {
      val v = new Mat[Double, OutR x InR](a._2.value.mmul(a._1.value.transpose()))
      shapeless.labelled.field[Sym](v) :: HNil
    }
  }


//////////////////////////////////////////////////////////////////
// Dense

  def compile[InR <: XInt, OutR <: XInt, OutC <: XInt](
    dag: Dense[Double, InR, OutR, OutC, ND4JAlgebra]
  ): ParametrisedFunction[
    Mat[Double, OutR x InR] :: HNil
  , Mat[Double, InR x OutC]
  , Mat[Double, OutR x OutC]
  ] = new ParametrisedFunction[
    Mat[Double, OutR x InR] :: HNil
  , Mat[Double, InR x OutC]
  , Mat[Double, OutR x OutC]
  ] {
    def apply(p: Mat[Double, OutR x InR] :: HNil, a: Mat[Double, InR x OutC]): Mat[Double, OutR x OutC] = {
      new Mat[Double, OutR x OutC](p.head.value.mmul(a.value))
    }
  }

  def grada[InR <: XInt, OutR <: XInt, OutC <: XInt](
    dag: Dense.Diff[Double, InR, OutR, OutC, ND4JAlgebra]
  ) = new ParametrisedFunction[
    Mat[Double, OutR x InR] :: HNil
  , (Mat[Double, InR x OutC], Mat[Double, OutR x OutC])
  , Mat[Double, InR x OutC]
  ] {
    def apply(
      p: Mat[Double, OutR x InR] :: HNil
    , a: (Mat[Double, InR x OutC], Mat[Double, OutR x OutC])
    ): Mat[Double, InR x OutC] = {
      new Mat[Double, InR x OutC](p.head.value.transpose().mmul(a._2.value))
    }
  }

  def gradp[InR <: XInt, OutR <: XInt, OutC <: XInt](
    dag: Dense.Diff[Double, InR, OutR, OutC, ND4JAlgebra]
  ) = new ParametrisedFunction[
    Mat[Double, OutR x InR] :: HNil
  , (Mat[Double, InR x OutC], Mat[Double, OutR x OutC])
  , Mat[Double, OutR x InR] :: HNil
  ] {
    def apply(
      p: Mat[Double, OutR x InR] :: HNil
    , a: (Mat[Double, InR x OutC], Mat[Double, OutR x OutC])
    ): Mat[Double, OutR x InR] :: HNil = {
      val v = new Mat[Double, OutR x InR](a._2.value.mmul(a._1.value.transpose()))
      v :: HNil
    }
  }


//////////////////////////////////////////////////////////////////
// Relu
  def compile[R <: XInt, C <: XInt](
    dag: Relu[Double, R, C, ND4JAlgebra]
  ) = new ParametrisedFunction[HNil, Mat[Double, R x C], Mat[Double, R x C]] {
    def apply(
      p: HNil
    , a: Mat[Double, R x C]
    ): Mat[Double, R x C] = {
      val relua = Nd4j.getExecutioner().execAndReturn(
        new org.nd4j.linalg.api.ops.impl.transforms.RectifedLinear(a.value)
      )

      new Mat[Double, R x C](relua)
    }
  }

  def gradp[R <: XInt, C <: XInt](
    dag: Relu.Diff[Double, R, C, ND4JAlgebra]
  ) = new ParametrisedFunction[HNil, (Mat[Double, R x C], Mat[Double, R x C]), HNil] {
    def apply(
      p: HNil
    , a: (Mat[Double, R x C], Mat[Double, R x C])
    ): HNil = HNil
  }

  def grada[R <: XInt : SafeInt, C <: XInt : SafeInt](
    dag: Relu.Diff[Double, R, C, ND4JAlgebra]
  ) = new ParametrisedFunction[HNil, (Mat[Double, R x C], Mat[Double, R x C]), Mat[Double, R x C]] {
    def apply(
      p: HNil
    , a: (Mat[Double, R x C], Mat[Double, R x C])
    ): Mat[Double, R x C] = {

      // in 0, relu is not differentiable => choosing a default value 1.0 (could be 0 or even 0.5)
      // 1 * a._2
      val zero = Nd4j.zeros(implicitly[SafeInt[R]], implicitly[SafeInt[C]])
      if(a._2.value.lt(0) == zero) a._2
      else new Mat[Double, R x C](zero)

    }
  }

//////////////////////////////////////////////////////////////////
// BiasMat
  def compile[Sym <: Singleton, R <: XInt, C <: XInt](
    dag: BiasMat[Sym, Double, R, C, ND4JAlgebra]
  ) = new ParametrisedFunction[FieldType[Sym, Mat[Double, R x C]] :: HNil, Mat[Double, R x C], Mat[Double, R x C]] {
    def apply(
      p: FieldType[Sym, Mat[Double, R x C]] :: HNil
    , a: Mat[Double, R x C]
    ): Mat[Double, R x C] = {
      new Mat[Double, R x C](p.head.value)
    }
  }

  def gradp[Sym <: Singleton, R <: XInt, C <: XInt](
    dag: BiasMat.Diff[Sym, Double, R, C, ND4JAlgebra]
  ) = new ParametrisedFunction[FieldType[Sym, Mat[Double, R x C]] :: HNil, (Mat[Double, R x C], Mat[Double, R x C]), FieldType[Sym, Mat[Double, R x C]] :: HNil] {
    def apply(
      p: FieldType[Sym, Mat[Double, R x C]] :: HNil
    , a: (Mat[Double, R x C], Mat[Double, R x C])
    ): FieldType[Sym, Mat[Double, R x C]] :: HNil = {
      shapeless.labelled.field[Sym](a._2) :: HNil
    }
  }


  def grada[Sym <: Singleton, R <: XInt : SafeInt, C <: XInt : SafeInt](
    dag: BiasMat.Diff[Sym, Double, R, C, ND4JAlgebra]
  ) = new ParametrisedFunction[FieldType[Sym, Mat[Double, R x C]] :: HNil, (Mat[Double, R x C], Mat[Double, R x C]), Mat[Double, R x C]] {
    def apply(
      p: FieldType[Sym, Mat[Double, R x C]] :: HNil
    , a: (Mat[Double, R x C], Mat[Double, R x C])
    ): Mat[Double, R x C] = {
      new Mat[Double, R x C](Nd4j.zeros(Array(implicitly[SafeInt[R]].value, implicitly[SafeInt[C]].value), Nd4j.order()))
    }
  }

  val fm = new org.nd4j.linalg.string.NDArrayStrings(8)

//////////////////////////////////////////////////////////////////
// L2

  def compile[D <: Dim](dag: CostL2[Double, D, ND4JAlgebra]) = 
    new ParametrisedFunction[HNil, (Mat[Double, D], Mat[Double, D]), Mat[Double, D]] {
      def apply(p: HNil, m: (Mat[Double, D], Mat[Double, D])): Mat[Double, D] = {
        val (x, y) = m
        val scoreArr = x.value.rsub(y.value)
        val r = scoreArr.muli(scoreArr).muli(0.5)
        new Mat[Double, D](r)
      }
    }

  def compile[D <: Dim](dag: CostL2Diff[Double, D, ND4JAlgebra]) = 
    new ParametrisedFunction[HNil, (Mat[Double, D], Mat[Double, D]), Mat[Double, D]] {
      def apply(p: HNil, m: (Mat[Double, D], Mat[Double, D])): Mat[Double, D] = {
        val (x, y) = m
        val r = x.value.sub(y.value)
        println(s"""l2diff
x:${fm.format(x.value)}
y:${fm.format(y.value)}
r:${fm.format(r)}
        """)
        new Mat(r)
      }
    }

  def compile[D <: Dim](dag: CostL2DiffInvert[Double, D, ND4JAlgebra]) = 
    new ParametrisedFunction[HNil, (Mat[Double, D], Mat[Double, D]), Mat[Double, D]] {
      def apply(p: HNil, m: (Mat[Double, D], Mat[Double, D])): Mat[Double, D] = {
        val (x, y) = m
        val r = x.value.sub(y.value)
        println(s"""l2diffinv
x:${fm.format(x.value)}
y:${fm.format(y.value)}
r:${fm.format(r)}
        """)
        new Mat(r)
      }
    }


//////////////////////////////////////////////////////////////////
// OPS

  def compile[D <: Dim](dag: ScalarTimes[Double, D, ND4JAlgebra]) = 
    new ParametrisedFunction[HNil, (Double, Mat[Double, D]), Mat[Double, D]] {
      def apply(p: HNil, m: (Double, Mat[Double, D])): Mat[Double, D] = {
        val (x, y) = m
        val r = y.value.mul(x)

        println(s"""scalartimes
x:${x}
y:${fm.format(y.value)}
r:${fm.format(r)}
        """)
        new Mat(r)
      }
    }

  def compile[D <: Dim](dag: Minus[D, Double, ND4JAlgebra]) = 
    new ParametrisedFunction[HNil, (Mat[Double, D], Mat[Double, D]), Mat[Double, D]] {
      def apply(p: HNil, m: (Mat[Double, D], Mat[Double, D])): Mat[Double, D] = {
        val (x, y) = m
        val r = x.value.rsub(y.value)
        println(s"""minus
x:${fm.format(x.value)}
y:${fm.format(y.value)}
r:${fm.format(r)}
        """)
        new Mat(r)
      }
    }

  def compile[A, B](dag: Func[A, B, Double, ND4JAlgebra]) = 
    new ParametrisedFunction[HNil, A, B] {
      def apply(p: HNil, a: A): B = {
        dag.f(a)
      }
    }
}

case class LearnCompiler(val learnRate: Double)
  extends ND4JAlgebra[Lambda[(p, a, b) => Dag[p, a, b, Double, ND4JAlgebra]]]
  with LearnCompiler0[Double, ND4JAlgebra] {

  val eps = learnRate

/////////////////////////////////////////////////////////////////////////////////////////////////////////
// SIGMOID
  def compile[R <: XInt, C <: XInt](
    dag: Sigmoid[Double, R, C, ND4JAlgebra]
  ): Dag[HNil, Mat[Double,R x C], Mat[Double, R x C], Double, ND4JAlgebra] = compile0(dag)

  def gradp[R <: XInt, C <: XInt](
    dag: Sigmoid.Diff[Double, R, C, ND4JAlgebra]
  ): Dag[HNil, (Mat[Double,R x C], Mat[Double, R x C]), HNil, Double, ND4JAlgebra] = gradp0(dag)

  def grada[R <: XInt, C <: XInt](
    dag: Sigmoid.Diff[Double, R, C, ND4JAlgebra]
  ): Dag[HNil, (Mat[Double,R x C], Mat[Double, R x C]), Mat[Double, R x C], Double, ND4JAlgebra] = grada0(dag)

/////////////////////////////////////////////////////////////////////////////////////////////////////////
// WEIGHTMAT
  def compile[Sym <: Singleton, InR <: XInt, OutR <: XInt, OutC <: XInt](
    dag: WeightMat[Sym, Double, InR, OutR, OutC, ND4JAlgebra]
  ): Dag[
    FieldType[Sym, Mat[Double, OutR x InR]] :: HNil
  , Mat[Double, InR x OutC]
  , Mat[Double, OutR x OutC], Double, ND4JAlgebra
  ] = compile0(dag)

  def gradp[Sym <: Singleton, InR <: XInt, OutR <: XInt, OutC <: XInt](
    dag: WeightMat.Diff[Sym, Double, InR, OutR, OutC, ND4JAlgebra]
  ): Dag[
    FieldType[Sym, Mat[Double, OutR x InR]] :: HNil
  , (Mat[Double, InR x OutC], Mat[Double, OutR x OutC])
  , FieldType[Sym, Mat[Double, OutR x InR]] :: HNil
  , Double, ND4JAlgebra
  ] = gradp0(dag)

  def grada[Sym <: Singleton, InR <: XInt, OutR <: XInt, OutC <: XInt](
    dag: WeightMat.Diff[Sym, Double, InR, OutR, OutC, ND4JAlgebra]
  ): Dag[
    FieldType[Sym, Mat[Double, OutR x InR]] :: HNil
  , (Mat[Double, InR x OutC], Mat[Double, OutR x OutC])
  , Mat[Double, InR x OutC]
  , Double, ND4JAlgebra
  ] = grada0(dag)

/////////////////////////////////////////////////////////////////////////////////////////////////////////
// DENSE
  def compile[InR <: XInt, OutR <: XInt, OutC <: XInt](
    dag: Dense[Double, InR, OutR, OutC, ND4JAlgebra]
  ): Dag[
    Mat[Double, OutR x InR] :: HNil
  , Mat[Double, InR x OutC]
  , Mat[Double, OutR x OutC], Double, ND4JAlgebra
  ] = compile0(dag)

  def gradp[InR <: XInt, OutR <: XInt, OutC <: XInt](
    dag: Dense.Diff[Double, InR, OutR, OutC, ND4JAlgebra]
  ): Dag[
    Mat[Double, OutR x InR] :: HNil
  , (Mat[Double, InR x OutC], Mat[Double, OutR x OutC])
  , Mat[Double, OutR x InR] :: HNil
  , Double, ND4JAlgebra
  ] = gradp0(dag)

  def grada[InR <: XInt, OutR <: XInt, OutC <: XInt](
    dag: Dense.Diff[Double, InR, OutR, OutC, ND4JAlgebra]
  ): Dag[
    Mat[Double, OutR x InR] :: HNil
  , (Mat[Double, InR x OutC], Mat[Double, OutR x OutC])
  , Mat[Double, InR x OutC]
  , Double, ND4JAlgebra
  ] = grada0(dag)


/////////////////////////////////////////////////////////////////////////////////////////////////////////
// RELU
  def compile[R <: XInt, C <: XInt](
    dag: Relu[Double, R, C, ND4JAlgebra]
  ): Dag[HNil, Mat[Double, R x C], Mat[Double, R x C], Double, ND4JAlgebra] = compile0(dag)

  def gradp[R <: XInt, C <: XInt](
    dag: Relu.Diff[Double, R, C, ND4JAlgebra]
  ): Dag[HNil, (Mat[Double, R x C], Mat[Double, R x C]), HNil, Double, ND4JAlgebra] = gradp0(dag)

  def grada[R <: XInt : SafeInt, C <: XInt : SafeInt](
    dag: Relu.Diff[Double, R, C, ND4JAlgebra]
  ): Dag[HNil, (Mat[Double, R x C], Mat[Double, R x C]), Mat[Double, R x C], Double, ND4JAlgebra] = grada0(dag)


/////////////////////////////////////////////////////////////////////////////////////////////////////////
// BIASMAT
  def compile[Sym <: Singleton, R <: XInt, C <: XInt](
    dag:  BiasMat[Sym, Double, R, C, ND4JAlgebra]
  ): Dag[FieldType[Sym, Mat[Double, R x C]] :: HNil, Mat[Double, R x C], Mat[Double, R x C], Double, ND4JAlgebra] = compile0(dag)

  def gradp[Sym <: Singleton, R <: XInt, C <: XInt](
    dag: BiasMat.Diff[Sym, Double, R, C, ND4JAlgebra]
  ): Dag[FieldType[Sym, Mat[Double, R x C]] :: HNil, (Mat[Double, R x C], Mat[Double, R x C]), FieldType[Sym, Mat[Double, R x C]] :: HNil, Double, ND4JAlgebra] = gradp0(dag)

  def grada[Sym <: Singleton, R <: XInt : SafeInt, C <: XInt : SafeInt](
    dag: BiasMat.Diff[Sym, Double, R, C, ND4JAlgebra]
  ): Dag[FieldType[Sym, Mat[Double, R x C]] :: HNil, (Mat[Double, R x C], Mat[Double, R x C]), Mat[Double, R x C], Double, ND4JAlgebra] = grada0(dag)


/////////////////////////////////////////////////////////////////////////////////////////////////////////
// COSTL2
  def compile[D <: neurocat.Dim](
    dag: CostL2Diff[Double, D, ND4JAlgebra]
  ): Dag[HNil,(Mat[Double, D],  Mat[Double, D]), Mat[Double, D], Double, ND4JAlgebra] = dag

  def compile[D <: neurocat.Dim](
    dag: CostL2[Double, D, ND4JAlgebra]
  ): Dag[HNil,(Mat[Double, D],  Mat[Double, D]), Mat[Double, D], Double, ND4JAlgebra] = dag

  def compile[D <: neurocat.Dim](
    dag: CostL2DiffInvert[Double, D, ND4JAlgebra]
  ): Dag[HNil,(Mat[Double, D],  Mat[Double, D]), Mat[Double, D], Double, ND4JAlgebra] = dag

  def compile[D <: neurocat.Dim](
    dag: ScalarTimes[Double, D, ND4JAlgebra]
  ): Dag[HNil,(Double,  Mat[Double, D]), Mat[Double, D], Double, ND4JAlgebra] = dag


//////////////////////////////////////////////////////////////////
// MINUS
  def compile[D <: Dim](
    dag: Minus[D, Double, ND4JAlgebra]
  ): Dag[HNil, (Mat[Double, D], Mat[Double, D]), Mat[Double, D], Double, ND4JAlgebra] =
    compile0(dag)

//////////////////////////////////////////////////////////////////
// FUNC
  def compile[A, B](
    dag: Func[A, B, Double, ND4JAlgebra]
  ): Dag[HNil, A, B, Double, ND4JAlgebra] =
    compile0(dag)
}


object ND4JCompiler
extends ND4JAlgebra[ParametrisedFunction]
with IdDiffCompiler
with ProdDiffCompiler
with ComposeDiffCompiler
with ConstDiffCompiler
with FstDiffCompiler
with SndDiffCompiler
with SplitDiffCompiler
with JoinDiffCompiler
with ND4JDiffCompiler
with ApplyCompiler
with SlideRDiffCompiler
with SlideLDiffCompiler
// with L2Compiler



trait Trainer[
  DataSet[row, nb]
] {
  def train[P, In, Out, NbSamples <: XInt : SafeInt](
    learner: ParametrisedFunction[P, (In, Out), P]
  )(
    initParams: P
  , trainingData: DataSet[(In, Out), NbSamples]
  , beforeEach: (P, In, Out) => Unit = { (p:P, i:In, o:Out) => () }
  , afterEach: (P, In, Out) => Unit = { (p:P, i:In, o:Out) => () }
  ): P
}

object Trainer {
  def naive[
    DataSet[row, nb]
  ](implicit rowTr: RowTraversable[DataSet]): Trainer[DataSet] =
    new Trainer[DataSet] {      
      def train[P, In, Out, NbSamples <: XInt : SafeInt](
        learner: ParametrisedFunction[P, (In, Out), P]
      )(
        initParams: P
      , trainingData: DataSet[(In, Out), NbSamples]
      , beforeEach: (P, In, Out) => Unit = { (p:P, i:In, o:Out) => () }
      , afterEach: (P, In, Out) => Unit = { (p:P, i:In, o:Out) => () }
      ): P = {
        var params = initParams

        rowTr.foreachRow(trainingData) {
          case (inRow, outRow) =>
            beforeEach(params, inRow, outRow)
            params = learner(params, (inRow, outRow))
            afterEach(params, inRow, outRow)
        }

        params
      }
    }
  }
