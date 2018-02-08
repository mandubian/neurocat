package neurocat
package idag5

import shapeless.{HList, HNil, ::, DepFn2}
import cats.Monoid

import algebra.ring.{MultiplicativeMonoid, AdditiveMonoid}

import shapeless.ops.record._

// import shapeless.record._
import shapeless.labelled._
import shapeless._ ; import syntax.singleton._ ; import record._

import singleton.ops._
import org.nd4j.linalg.factory.Nd4j
import nd4j._

trait ParametrisedFunction[P, A, B] extends Function2[P, A, B]

trait ParametrisedDiff[P, A, B, GradP, GradA] extends ParametrisedFunction[P, A, B] {
  def gradP(p: P, a: A): GradP
  def gradA(p: P, a: A): GradA
}

sealed trait PDag {
  type P
  type A
  type B
}

object PDag {
  type Aux[P0, A0, B0] = PDag { type P = P0; type A = A0; type B = B0 }
}

// trait ParametrisedDag[P0, A0, B0] extends PDag {
//   type P = P0
//   type A = A0
//   type B = B0
// }

sealed trait DiffPDag extends PDag {
  type P
  type A
  type B
  type GradP
  type GradA
}

trait Merger[P, Q]{
  type PQ
  def left(p: PQ): P
  def right(p: PQ): Q
  def apply(p: P, q: Q): PQ
} 

object Merger {
  type Aux[P, Q, PQ0] = Merger[P, Q] { type PQ = PQ0 }

  implicit def hlistMerger[P <: HList, Q <: HList](
    implicit m: MergerE[P, Q]
  ): Merger.Aux[P, Q, m.Out] = new Merger[P, Q] {
    type PQ = m.Out

    def left(pq: PQ): P = m.left(pq)
    def right(pq: PQ): Q = m.right(pq)
    def apply(p: P, q: Q): PQ = m(p, q)
  }
}

case class ParallelDiffPDag[
  F <: DiffPDag
, G <: DiffPDag
, PQ
, GP
](
  val f: F
, val g: G
) extends DiffPDag {
  type P = PQ
  type A = (f.A, g.A)
  type B = (f.B, g.B)
  type GradP = GP
  type GradA = (f.GradA, g.GradA)
}

final case class ComposeDiffPDag[
  F <: DiffPDag
, G <: DiffPDag
, PQ
, GP
](
  g: G, f: F
) extends DiffPDag {
  type P = PQ
  type A = f.A
  type B = g.B
  type GradP = GP
  type GradA = (f.GradA, g.GradA)
}


trait Dag[A, B] extends DiffPDag {
  type P = HNil
  type A
  type B
  type GradP = HNil
  type GradA = Void
}


final case class GradP[F <: DiffPDag](f: F) extends DiffPDag {
  type P = f.P
  type A = f.A
  type B = f.GradP
  type GradP = HNil
  type GradA = Void
}

final case class GradA[F <: DiffPDag](f: F) extends DiffPDag {
  type P = f.P
  type A = f.A
  type B = f.GradA
  type GradP = HNil
  type GradA = Void
}

final case class Const[S](s: S) extends DiffPDag {
  type P = HNil
  type A = HNil
  type B = S
  type GradP = HNil
  type GradA = Void
}

// extensible
trait ExtDiffPDag extends DiffPDag {
  type P
  type A
  type B
  type GradP
  type GradA
}

case class Sigmoid[R, D <: Dim2[_, _]]()
  extends ExtDiffPDag {
    type P = HNil
    type A = Mat[R, D]
    type B = Mat[R, D]
    type GradP = HNil
    type GradA = Mat[R, D]
  }

object Sigmoid {

  implicit def exploder[R, D <: Dim2[_, _]] =
    new Exploder[Sigmoid[R, D]] {
      type P = HNil
      type A = Mat[R, D]
      type B = Mat[R, D]
      type GradP = HNil
      type GradA = Mat[R, D]
    }

}

class DenseLayer[S <: Singleton, R, InR <: XInt, OutR <: XInt, OutC <: XInt]()
  extends ExtDiffPDag {
    type P = FieldType[S, Mat[R, OutR x InR]] :: HNil
    type A = Mat[R, InR x OutC]
    type B = Mat[R, OutR x OutC]
    type GradP = FieldType[S, Mat[R, InR x OutC]] :: HNil
    type GradA = Mat[R, OutR x InR]
  }

object DenseLayer {

  def apply[S <: Singleton, R, In <: Dim2[_, _], Out <: Dim2[_, _]](
    implicit same: Dim2SameCol[In, Out]
  ): DenseLayer[S, R, same.In, same.OutR, same.OutC] = new DenseLayer[S, R, same.In, same.OutR, same.OutC]()


  implicit def exploder[S <: Singleton, R, InR <: XInt, OutR <: XInt, OutC <: XInt] =
    new Exploder[DenseLayer[S, R, InR, OutR, OutC]] {
      type P = FieldType[S, Mat[R, OutR x InR]] :: HNil
      type A = Mat[R, InR x OutC]
      type B = Mat[R, OutR x OutC]
      type GradP = FieldType[S, Mat[R, InR x OutC]] :: HNil
      type GradA = Mat[R, OutR x InR]
    }
}

class DenseLayer2[R, InR <: XInt, OutR <: XInt, OutC <: XInt]()
  extends ExtDiffPDag {
    type P = Mat[R, OutR x InR] :: HNil
    type A = Mat[R, InR x OutC]
    type B = Mat[R, OutR x OutC]
    type GradP = Mat[R, InR x OutC] :: HNil
    type GradA = Mat[R, OutR x InR]
  }

object DenseLayer2 {

  def apply[R, In <: Dim2[_, _], Out <: Dim2[_, _]](
    implicit same: Dim2SameCol[In, Out]
  ): DenseLayer2[R, same.In, same.OutR, same.OutC] = new DenseLayer2[R, same.In, same.OutR, same.OutC]()

  implicit def exploder[R, InR <: XInt, OutR <: XInt, OutC <: XInt] =
    new Exploder[DenseLayer2[R, InR, OutR, OutC]] {
      type P = Mat[R, OutR x InR] :: HNil
      type A = Mat[R, InR x OutC]
      type B = Mat[R, OutR x OutC]
      type GradP = Mat[R, InR x OutC] :: HNil
      type GradA = Mat[R, OutR x InR]
    }
}


case class Id[A0]() extends ExtDiffPDag {
  type P = HNil
  type A = A0
  type B = A0
  type GradP = HNil
  type GradA = Unit
}

object Id {
  implicit def exploder[A0] = new Exploder[Id[A0]] {
    type P = HNil
    type A = A0
    type B = A0
    type GradP = HNil
    type GradA = Unit
  }
}

trait Parallelizable[F, G] {
  type Out
  def apply(f: F, g: G): Out
}

object Parallelizable {
  type Aux[F, G, O] = Parallelizable[F, G] { type Out = O }

  implicit def par1[
    P, A, B, GP, GA1
  , Q, C, D, GQ, GA2
  , F <: DiffPDag
  , G <: DiffPDag
  ](implicit
    f: Exploder.Aux[F, P, A, B, GP, GA1]
  , g: Exploder.Aux[G, Q, C, D, GQ, GA2]
  , MergerPQ0: Merger[P, Q], MergerGradPQ0: Merger[GP, GQ]
  ) : Parallelizable.Aux[F, G, ParallelDiffPDag[F, G, MergerPQ0.PQ, MergerGradPQ0.PQ]] = new Parallelizable[F, G] {
    type Out = ParallelDiffPDag[F, G, MergerPQ0.PQ, MergerGradPQ0.PQ]

    def apply(f: F, g: G): Out = ParallelDiffPDag[F, G, MergerPQ0.PQ, MergerGradPQ0.PQ](f, g)
  }

}

@annotation.implicitNotFound(msg = "${F} do not compose with ${G}")
trait Composable[F, G] {
  type Out
  def apply(g: G, f: F): Out
}


object Composable {
  type Aux[F, G, O] = Composable[F, G] {
    type Out = O
  }

  implicit def comp1[
    A0, B, C
  , P, GP, GA1
  , Q, GQ, GA2
  , F <: DiffPDag
  , G <: DiffPDag
  ](implicit
    f: Exploder.Aux[F, P, A0, B, GP, GA1]
  , g: Exploder.Aux[G, Q, B, C, GQ, GA2]
  , MergerPQ0: Merger[P, Q], MergerGradPQ0: Merger[GP, GQ]
  ) : Composable.Aux[F, G, ComposeDiffPDag[F, G, MergerPQ0.PQ, MergerGradPQ0.PQ]] = new Composable[F, G] {
    type Out = ComposeDiffPDag[F, G, MergerPQ0.PQ, MergerGradPQ0.PQ]

    def apply(g: G, f: F): Out = ComposeDiffPDag[F, G, MergerPQ0.PQ, MergerGradPQ0.PQ](g, f)
  }

}


trait Exploder0[F] {
  type P
  type A
  type B
}

object Exploder0 {
  type Aux[F, P0, A0, B0] = Exploder0[F] { type A = A0; type B = B0; type P = P0 }
}

trait Exploder[F <: DiffPDag] {
  type P
  type A
  type B
  type GradP
  type GradA
}

object Exploder {
  // type Aux[F <: DiffPDag, P0, GP] = Exploder[F] { type P = P0; type GradP = GP }
  type Aux[F <: DiffPDag, P0, A0, B0, GP, GA] = Exploder[F] { type A = A0; type B = B0; type P = P0; type GradP = GP; type GradA = GA }

  // implicit def ext[P0, A0, B0, GP0, GA0, F <: ExtDiffPDag[P0, A0, B0, GP0, GA0]]: Exploder.Aux[F, P0, GP0] = new Exploder[F] {
  //   type P = P0
  //   type A = A0
  //   type B = B0
  //   type GradP = GP0
  //   type GradA = GA0
  // }

  implicit def parallel[
    F <: DiffPDag
  , G <: DiffPDag
  , P, A0, C0, GP1, GA1
  , Q, B0, D0, GP2, GA2
  , PQ, GPQ
  ](implicit
    f: Exploder.Aux[F, P, A0, C0, GP1, GA1]
  , g: Exploder.Aux[G, Q, B0, D0, GP2, GA2]
  , MergerPQ0: Merger.Aux[P, Q, PQ]
  , MergerGradPQ0: Merger.Aux[GP1, GP2, GPQ]
  ): Exploder.Aux[ParallelDiffPDag[F, G, PQ, GPQ], PQ, (A0, B0), (C0, D0), GPQ, (GA1, GA2)] =
    new Exploder[ParallelDiffPDag[F, G, PQ, GPQ]] {
      type P = PQ
      type A = (A0, B0)
      type B = (C0, D0)
      type GradP = GPQ
      type GradA = (GA1, GA2)  
    }

  implicit def compose[
    F <: DiffPDag
  , G <: DiffPDag
  , P, A0, B0, GP1, GA1
  , Q, C0, GP2, GA2
  , PQ, GPQ
  ](implicit
    f: Exploder.Aux[F, P, A0, B0, GP1, GA1]
  , g: Exploder.Aux[G, Q, B0, C0, GP2, GA2]
  , MergerPQ0: Merger.Aux[P, Q, PQ]
  , MergerGradPQ0: Merger.Aux[GP1, GP2, GPQ]
  ): Exploder.Aux[ComposeDiffPDag[F, G, PQ, GPQ], PQ, A0, C0, GPQ, (GA1, GA2)] =
    new Exploder[ComposeDiffPDag[F, G, PQ, GPQ]] {
      type P = PQ
      type A = A0
      type B = C0
      type GradP = GPQ
      type GradA = (GA1, GA2)
    }

}



object DiffPDag {
  type Aux[P0, A0, B0, GradP0, GradA0] = DiffPDag {
    type P = P0
    type A = A0
    type B = B0
    type GradP = GradP0
    type GradA = GradA0
  }

  implicit class Ops[F <: DiffPDag](f: F) {

    def ||[G <: DiffPDag](g: G)(
      implicit par: Parallelizable[F, G]
    ): par.Out = par(f, g)

    def >>>[G <: DiffPDag](g: G)(
      implicit comp: Composable[F, G]
    ): comp.Out = comp(g, f)
  }

  // def pp[R, D <: Dim2[_, _]] = implicitly[Exploder.Aux[Sigmoid[R, D], HNil, HNil]]
  // def pp[R, InR <: XInt, OutR <: XInt, OutC <: XInt] = implicitly[
  //   Exploder.Aux[
  //     DenseLayer["s",R,InR,OutR,OutC]
  //   , FieldType["s", Mat[R, OutR x InR]] :: HNil
  //   , Mat[R, InR x OutC], Mat[R, OutR x OutC]
  //   , FieldType["s", Mat[R, InR x OutC]] :: HNil
  //   ]
  // ]

  // def toto[R, InR <: XInt, OutR <: XInt, OutC <: XInt] = implicitly[
  //   Parallelizable[DenseLayer["s",R,InR,OutR,OutC], DenseLayer["t",R,InR,OutR,OutC]]
  // ]

  // implicit def exploder[S <: Singleton, R, InR <: XInt, OutR <: XInt, OutC <: XInt] =
  //   new Exploder[DenseLayer[S, R, InR, OutR, OutC]] {
  //     type P = FieldType[S, Mat[R, OutR x InR]] :: HNil
  //     type A = Mat[R, InR x OutC]
  //     type B = Mat[R, OutR x OutC]
  //     type GradP = FieldType[S, Mat[R, InR x OutC]] :: HNil
  //     type GradA = Mat[R, OutR x InR]
  //   }

  // def pp0[R, InR <: XInt, OutR <: XInt, OutC <: XInt]: Exploder[
  //   ParallelDiffPDag[
  //     DenseLayer["s",R,InR,OutR,OutC]
  //   , DenseLayer["s",R,InR,OutR,OutC]
  //   , FieldType["s", Mat[R, OutR x InR]] :: HNil
  //   , FieldType["s", Mat[R, InR x OutC]] :: HNil
  //   ]
  // ] = Exploder.parallel[
  //   DenseLayer["s",R,InR,OutR,OutC]
  // , DenseLayer["s",R,InR,OutR,OutC]
  // , FieldType["s", Mat[R, OutR x InR]] :: HNil, Mat[R, InR x OutC], Mat[R, OutR x OutC], FieldType["s", Mat[R, InR x OutC]] :: HNil
  // , FieldType["s", Mat[R, OutR x InR]] :: HNil, Mat[R, InR x OutC], Mat[R, OutR x OutC], FieldType["s", Mat[R, InR x OutC]] :: HNil
  // ]
  // def pp[R, InR <: XInt, OutR <: XInt, OutC <: XInt] = implicitly[
  //   Exploder[
  //     ParallelDiffPDag[
  //       DenseLayer["s",R,InR,OutR,OutC]
  //     , DenseLayer["s",R,InR,OutR,OutC]
  //     , FieldType["s", Mat[R, OutR x InR]] :: HNil
  //     , FieldType["s", Mat[R, InR x OutC]] :: HNil
  //     ]
  //   ]
  // ]

  trait Contextualized[Ctx, D] {
    type Out
    def apply(f: D): Out
  }
  object Contextualized {
    type Aux[Ctx, D, O] = Contextualized[Ctx, D] { type Out = O }
  }

  trait ND4J
  case object ND4J extends ND4J

  implicit def nd4jParallel[
    Ctx
  , F <: DiffPDag, PF, AF, BF, GPF, GAF
  , G <: DiffPDag, PG, AG, BG, GPG, GAG
  , P, GP
  ](
    implicit
      fe: Exploder.Aux[F, PF, AF, BF, GPF, GAF]
    , f: Contextualized.Aux[Ctx, F, ParametrisedDiff[PF, AF, BF, GPF, GAF]]
    , ge: Exploder.Aux[G, PG, AG, BG, GPG, GAG]
    , g: Contextualized.Aux[Ctx, G, ParametrisedDiff[PG, AG, BG, GPG, GAG]]
    , MergerPQ: Merger.Aux[PF, PG, P]
    , MergerGradPQ: Merger.Aux[GPF, GPG, GP]
  ) = new Contextualized[Ctx, ParallelDiffPDag[F, G, P, GP]] {
    type Out = ParametrisedDiff[P, (AF, AG), (BF, BG), GP, (GAF, GAG)]
    def apply(par: ParallelDiffPDag[F, G, P, GP]): ParametrisedDiff[P, (AF, AG), (BF, BG), GP, (GAF, GAG)] =
      new ParametrisedDiff[P, (AF, AG), (BF, BG), GP, (GAF, GAG)] {
        val parf = f(par.f)
        val parg = g(par.g)
        def apply(pq: P, ab: (AF, AG)): (BF, BG) = {
          (parf(MergerPQ.left(pq), ab._1), parg(MergerPQ.right(pq), ab._2))
        }

        type GradP = GP
        def gradP(pq: P, ab: (AF, AG)): GP =
          MergerGradPQ(parf.gradP(MergerPQ.left(pq), ab._1), parg.gradP(MergerPQ.right(pq), ab._2))

        type GradA = (GAF, GAG)
        def gradA(pq: P, ab: (AF, AG)): (GAF, GAG) =
          (parf.gradA(MergerPQ.left(pq), ab._1), parg.gradA(MergerPQ.right(pq), ab._2))
      }
  }

  implicit def nd4jCompose[
    Ctx
  , F <: DiffPDag, PF, AF, BF, GPF, GAF
  , G <: DiffPDag, PG, BG, GPG, GAG
  , P, GP
  ](
    implicit
      fe: Exploder.Aux[F, PF, AF, BF, GPF, GAF]
    , f: Contextualized.Aux[Ctx, F, ParametrisedDiff[PF, AF, BF, GPF, GAF]]
    , ge: Exploder.Aux[G, PG, BF, BG, GPG, GAG]
    , g: Contextualized.Aux[Ctx, G, ParametrisedDiff[PG, BF, BG, GPG, GAG]]
    , MergerPQ: Merger.Aux[PF, PG, P]
    , MergerGradPQ: Merger.Aux[GPF, GPG, GP]
  ) = new Contextualized[Ctx, ComposeDiffPDag[F, G, P, GP]] {
    type Out = ParametrisedDiff[P, AF, BG, GP, (GAF, GAG)]
    def apply(par: ComposeDiffPDag[F, G, P, GP]): ParametrisedDiff[P, AF, BG, GP, (GAF, GAG)] =
      new ParametrisedDiff[P, AF, BG, GP, (GAF, GAG)] {
        val parf = f(par.f)
        val parg = g(par.g)
        def apply(pq: P, ab: AF): BG = {
          parg(MergerPQ.right(pq), parf(MergerPQ.left(pq), ab))
        }

        type GradP = GP
        def gradP(pq: P, ab: AF): GP =
          MergerGradPQ(parf.gradP(MergerPQ.left(pq), ab), parg.gradP(MergerPQ.right(pq), parf(MergerPQ.left(pq), ab)))

        type GradA = (GAF, GAG)
        def gradA(pq: P, ab: AF): (GAF, GAG) =
          (parf.gradA(MergerPQ.left(pq), ab), parg.gradA(MergerPQ.right(pq), parf(MergerPQ.left(pq), ab)))
      }
  }

  implicit def nd4jSigmoid[R, D <: Dim2[_, _]] = new Contextualized[
    ND4J
  , Sigmoid[R, D]
  ] {
    type Out = ParametrisedDiff[HNil, Mat[R, D], Mat[R, D], HNil, Mat[R, D]]
    def apply(f: Sigmoid[R, D]): ParametrisedDiff[HNil, Mat[R, D], Mat[R, D], HNil, Mat[R, D]] =
      new ParametrisedDiff[HNil, Mat[R, D], Mat[R, D], HNil, Mat[R, D]] {
        def apply(p: HNil, a: Mat[R, D]) = new Mat[R, D](
          Nd4j.getExecutioner().execAndReturn(new org.nd4j.linalg.api.ops.impl.transforms.Sigmoid(a.value))
        )

        def gradP(p: HNil, a: Mat[R, D]): HNil = HNil

        def gradA(p: HNil, a: Mat[R, D]) = new Mat[R, D](
          Nd4j.getExecutioner().execAndReturn(new org.nd4j.linalg.api.ops.impl.transforms.SigmoidDerivative(a.value))
        )
      }
  }

  implicit def nd4jDenseLayer[S <: Singleton, R, InR <: XInt, OutR <: XInt, OutC <: XInt] = new Contextualized[
    ND4J
  , DenseLayer[S, R, InR, OutR, OutC]
  ] {
    type Out = ParametrisedDiff[
      FieldType[S, Mat[R, OutR x InR]] :: HNil
    , Mat[R, InR x OutC]
    , Mat[R, OutR x OutC]
    , FieldType[S, Mat[R, InR x OutC]] :: HNil
    , Mat[R, OutR x InR]
    ]
    def apply(f: DenseLayer[S, R, InR, OutR, OutC]): Out =
      new ParametrisedDiff[
        FieldType[S, Mat[R, OutR x InR]] :: HNil
      , Mat[R, InR x OutC]
      , Mat[R, OutR x OutC]
      , FieldType[S, Mat[R, InR x OutC]] :: HNil
      , Mat[R, OutR x InR]
      ] {
        def apply(weights: FieldType[S, Mat[R, OutR x InR]] :: HNil, in: Mat[R, InR x OutC]): Mat[R, OutR x OutC] = {
          new Mat[R, OutR x OutC](weights.head.value.mmul(in.value))
        }

        def gradP(weights: FieldType[S, Mat[R, OutR x InR]] :: HNil, in: Mat[R, InR x OutC]): FieldType[S, Mat[R, InR x OutC]] :: HNil = {
          shapeless.labelled.field[S](in) :: HNil
        }

        def gradA(weights: FieldType[S, Mat[R, OutR x InR]] :: HNil, in: Mat[R, InR x OutC]): Mat[R, OutR x InR] = {
          weights.head
        }
      }
  }

  trait Contextualizer[Ctx] {
    def apply[D, F](f: D)(
      implicit cc: Contextualized[Ctx, D]
    ): cc.Out = cc(f)
  }

  def contextualize[Ctx] = new Contextualizer[Ctx] {}

  val a = Mat.randomD2[Double, 2 x 3](min = 0.0, max = 1.0)

  val f = contextualize[ND4J](Sigmoid[Double, 2 x 3]())

  val g = contextualize[ND4J](DenseLayer["s", Double, 1 x 3, 1 x 3])
  
  def nnetlayer[R, InR <: XInt, OutR <: XInt, OutC <: XInt] = 
    // DenseLayer["s", R, InR x OutC, OutR x OutC] || DenseLayer["s", R, InR x OutC, OutR x OutC]
    (DenseLayer["s", R, InR x OutC, OutR x OutC] >>> Sigmoid[R, OutR x OutC]()) ||
      (DenseLayer["t", R, InR x OutC, OutR x OutC] >>> Sigmoid[R, OutR x OutC]())

    // (DenseLayer["s", R, InR x OutC, OutR x OutC] || DenseLayer["s", R, InR x OutC, OutR x OutC]) >>>
    //   (Sigmoid[R, OutR x OutC]() || Sigmoid[R, OutR x OutC]())

  def fnnet[R, InR <: XInt, OutR <: XInt, OutC <: XInt] =
    contextualize[ND4J](nnetlayer[R, InR, OutR, OutC])
}
