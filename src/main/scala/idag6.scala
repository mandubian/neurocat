package neurocat
package idag6

import shapeless.{HList, HNil, ::, DepFn2}
import cats.Monoid

import algebra.ring.{MultiplicativeMonoid, AdditiveMonoid}

import shapeless.ops.record._

// import shapeless.record._
import shapeless.labelled._
import shapeless._ ; import syntax.singleton._ ; import record._

import cats.evidence._

import singleton.ops._
import org.nd4j.linalg.factory.Nd4j
import nd4j._


sealed trait PDag[P, A, B, GradP, GradA, V[p, a, b, gp, ga, r] <: PDag.Visitor[p, a, b, gp, ga, r]] {

  type Visitor[R] = V[P, A, B, GradP, GradA, R]

  // the ugly GADT patch
  def accept[R](visitor: Visitor[R]): R

}

object PDag {
  abstract class ParallelPDag[
    P, PA, PB, PGP, PGA
  , Q, QA, QB, QGP, QGA
  , PQ
  , PQGP
  , V[p, a, b, gp, ga, r] <: PDag.Visitor[p, a, b, gp, ga, r]
  ] private[neurocat] (
    f: PDag[P, PA, PB, PGP, PGA, V]
  , g: PDag[Q, QA, QB, QGP, QGA, V]
  ) extends PDag[
    PQ, (PA, QA), (PB, QB), PQGP, (PGA, QGA), V
  ] {

    def merger: Merger.Aux[P, Q, PQ]
    def mergerg: Merger.Aux[PGP, QGP, PQGP]

    def accept[R](visitor: Visitor[R]): R = visitor(this)

  }

  // object ParallelPDag {
  //   def apply[
  //     P, PA, PB, PGP, PGA
  //   , Q, QA, QB, QGP, QGA
  //   ]
  // }

  abstract class ComposePDag[
    P, A, B, PGP, PGA
  , Q, C, QGP, QGA
  , PQ
  , PQGP
  , V[p, a, b, gp, ga, r] <: PDag.Visitor[p, a, b, gp, ga, r]
  ] private[neurocat] (
    g: PDag[Q, B, C, QGP, QGA, V]
  , f: PDag[P, A, B, PGP, PGA, V]
  ) extends PDag[PQ, A, C, PQGP, (PGA, QGA), V] {

    def merger: Merger.Aux[P, Q, PQ]
    def mergerg: Merger.Aux[PGP, QGP, PQGP]

    def accept[R](visitor: Visitor[R]): R = visitor(this)

  }


  case class Id[A, V[p, a, b, gp, ga, r] <: PDag.Visitor[p, a, b, gp, ga, r]]()
  extends PDag[HNil, A, A, HNil, Unit, V] {

    def accept[R](visitor: Visitor[R]): R = visitor(this)

  }

  sealed trait Dag[A, B, V[p, a, b, gp, ga, r] <: PDag.Visitor[p, a, b, gp, ga, r]]
    extends PDag[HNil, A, B, HNil, Void, V]

  case class GradPDag[
    P, A, B, PGP, PGA
  , V[p, a, b, gp, ga, r] <: PDag.Visitor[p, a, b, gp, ga, r]
  ](dag: PDag[P, A, B, PGP, PGA, V]) extends Dag[(P, A), PGP, V]  {

    def accept[R](visitor: Visitor[R]): R = visitor(this)

  }

  case class GradADag[
    P, A, B, PGP, PGA
  , V[p, a, b, gp, ga, r] <: PDag.Visitor[p, a, b, gp, ga, r]
  ](dag: PDag[P, A, B, PGP, PGA, V]) extends Dag[(P, A), PGA, V] {

    def accept[R](visitor: Visitor[R]): R = visitor(this)

  }



  trait ExtPDag[P, A, B, GP, GA, V[p, a, b, gp, ga, r] <: PDag.Visitor[p, a, b, gp, ga, r]]
    extends PDag[P, A, B, GP, GA, V]

  trait Visitor[RP, RA, RB, RGP, RGA, R] {
    self =>

    def apply[X, P, PGP, PGA, Q, QGP, QGA, V[p, a, b, gp, ga, r] <: PDag.Visitor[p, a, b, gp, ga, r]](
      f: ComposePDag[P, RA, X, PGP, PGA, Q, RB, QGP, QGA, RP, RGP, V]
    )(
      implicit
        ev: (PGA, QGA) === RGA
    ): R

    def apply[P, PA, PB, PGP, PGA, Q, QA, QB, QGP, QGA, V[p, a, b, gp, ga, r] <: PDag.Visitor[p, a, b, gp, ga, r]](
      f: ParallelPDag[P, PA, PB, PGP, PGA, Q, QA, QB, QGP, QGA, RP, RGP, V]
    )(
      implicit
        ev1: (PA, QA) === RA
      , ev2: (PB, QB) === RB
      , ev3: (PGA, QGA) === RGA
    ): R

    def apply[V[p, a, b, gp, ga, r] <: PDag.Visitor[p, a, b, gp, ga, r]](
      f: Id[RA, V]
    )(implicit
      ev1: HNil === RP
    , ev2: RA === RB
    , ev3: HNil === RGP
    , ev4: Unit === RGA
    ): R

    def apply[
      P, A, B, GP, GA
    , V[p, a, b, gp, ga, r] <: PDag.Visitor[p, a, b, gp, ga, r]
    ](
      f: GradPDag[P, A, B, GP, GA, V]
    )(implicit
      ev1: HNil === RP
    , ev2: (P, A) === RA
    , ev3: GP === RB
    , ev4: HNil === RGP
    , ev5: Void === RGA
    ): R

    def apply[
      P, A, B, GP, GA
    , V[p, a, b, gp, ga, r] <: PDag.Visitor[p, a, b, gp, ga, r]
    ](
      f: GradADag[P, A, B, GP, GA, V]
    )(implicit
      ev1: HNil === RP
    , ev2: (P, A) === RA
    , ev3: GA === RB
    , ev4: HNil === RGP
    , ev5: Void === RGA
    ): R
  }

  def par[
    P, PA, PB, PGP, PGA
  , Q, QA, QB, QGP, QGA
  , V[p, a, b, gp, ga, r] <: PDag.Visitor[p, a, b, gp, ga, r]
  ](f: PDag[P, PA, PB, PGP, PGA, V], g: PDag[Q, QA, QB, QGP, QGA, V])(
    implicit merger0: Merger[P, Q], mergerg0: Merger[PGP, QGP]
  ) = new ParallelPDag[
        P, PA, PB, PGP, PGA
      , Q, QA, QB, QGP, QGA
      , merger0.Out, mergerg0.Out, V](f, g) {
        def merger = merger0
        def mergerg = mergerg0
      }


  def compose[
    P, A, B, PGP, PGA
  , Q, C, QGP, QGA
  , V[p, a, b, gp, ga, r] <: PDag.Visitor[p, a, b, gp, ga, r]
  ](g: PDag[Q, B, C, QGP, QGA, V], f: PDag[P, A, B, PGP, PGA, V])(
    implicit merger0: Merger[P, Q], mergerg0: Merger[PGP, QGP]
  ) = new ComposePDag[
        P, A, B, PGP, PGA
      , Q, C, QGP, QGA
      , merger0.Out, mergerg0.Out, V](g, f) {
        def merger = merger0
        def mergerg = mergerg0
      }


  implicit class Ops[P, PA, PB, PGP, PGA, V[p, a, b, gp, ga, r] <: PDag.Visitor[p, a, b, gp, ga, r]](
    idag: PDag[P, PA, PB, PGP, PGA, V]
  ) {
    def ||[Q, QA, QB, QGP, QGA](other: PDag[Q, QA, QB, QGP, QGA, V])(
      implicit merger: Merger[P, Q], mergerg: Merger[PGP, QGP]
    ): PDag[merger.Out, (PA, QA), (PB, QB), mergerg.Out, (PGA, QGA), V] = par(idag, other)

    def >>>[Q, QC, QGP, QGA](other: PDag[Q, PB, QC, QGP, QGA, V])(
      implicit merger: Merger[P, Q], mergerg: Merger[PGP, QGP]
    ): PDag[merger.Out, PA, QC, mergerg.Out, (PGA, QGA), V] = compose(other, idag)

    def compile[Ctx](implicit cc: Transform[Ctx, PDag[P, PA, PB, PGP, PGA, V]]): cc.Out = cc(idag)
  }
}

trait Merger[P, Q]{
  type Out
  def left(p: Out): P
  def right(p: Out): Q
  def apply(p: P, q: Q): Out
} 

object Merger {
  type Aux[P, Q, PQ0] = Merger[P, Q] { type Out = PQ0 }

  implicit def hlistMerger[P <: HList, Q <: HList](
    implicit m: MergerE[P, Q]
  ): Merger.Aux[P, Q, m.Out] = new Merger[P, Q] {
    type Out = m.Out

    def left(pq: Out): P = m.left(pq)
    def right(pq: Out): Q = m.right(pq)
    def apply(p: P, q: Q): Out = m(p, q)
  }
}

trait ParametrisedFunction[P, A, B] extends Function2[P, A, B]

trait ParametrisedDiff[P, A, B, GradP, GradA] extends ParametrisedFunction[P, A, B] {
  def gradP(p: P, a: A): GradP
  def gradA(p: P, a: A): GradA
}

object NNet {

  case class Sigmoid[R, D <: Dim2[_, _], V[p, a, b, gp, ga, r] <: NNet.Visitor[p, a, b, gp, ga, r]]()
  extends PDag.ExtPDag[HNil, Mat[R, D], Mat[R, D], HNil, Mat[R, D], V] {

    def accept[R](visitor: Visitor[R]): R = visitor(this)
  }

  class DenseLayer[
    S <: Singleton, R, InR <: XInt, OutR <: XInt, OutC <: XInt
  , V[p, a, b, gp, ga, r] <: NNet.Visitor[p, a, b, gp, ga, r]
  ]()
    extends PDag.ExtPDag[
      FieldType[S, Mat[R, OutR x InR]] :: HNil, Mat[R, InR x OutC]
    , Mat[R, OutR x OutC]
    , FieldType[S, Mat[R, InR x OutC]] :: HNil
    , Mat[R, OutR x InR]
    , V
    ] {
      def accept[RR](visitor: Visitor[RR]): RR = visitor(this)
    }


  trait Visitor[RP, RA, RB, RGP, RGA, Out] extends PDag.Visitor[RP, RA, RB, RGP, RGA, Out] {
    def apply[S, D <: Dim2[_, _], V[p, a, b, gp, ga, r] <: NNet.Visitor[p, a, b, gp, ga, r]](
      sigmoid: Sigmoid[S, D, V]
    )(implicit
      ev1: HNil === RP
    , ev2: Mat[S, D] === RA
    , ev3: RA === RB
    , ev4: HNil === RGP
    , ev5: Mat[S, D] === RGA
    ): Out

    def apply[
      S <: Singleton, R, InR <: XInt, OutR <: XInt, OutC <: XInt
    , V[p, a, b, gp, ga, r] <: NNet.Visitor[p, a, b, gp, ga, r]
    ](
      dense: DenseLayer[S, R, InR, OutR, OutC, V]
    )(implicit
      ev1: (FieldType[S, Mat[R, OutR x InR]] :: HNil) === RP
    , ev2: Mat[R, InR x OutC] === RA
    , ev3: Mat[R, OutR x OutC] === RB
    , ev4: (FieldType[S, Mat[R, InR x OutC]] :: HNil) === RGP
    , ev5: Mat[R, OutR x InR] === RGA
    ): Out

  }

}
trait ND4J
case object ND4J extends ND4J {

}


trait DagDsl[V[p, a, b, gp, ga, r] <: PDag.Visitor[p, a, b, gp, ga, r]] {
  type ParallelPDag[
    P, PA, PB, PGP, PGA
  , Q, QA, QB, QGP, QGA
  , PQ
  , PQGP
  ] = PDag.ParallelPDag[
        P, PA, PB, PGP, PGA
      , Q, QA, QB, QGP, QGA
      , PQ
      , PQGP
      , V
      ]

  type ComposePDag[
    P, A, B, PGP, PGA
  , Q, C, QGP, QGA
  , PQ
  , PQGP
  ] = PDag.ComposePDag[
        P, A, B, PGP, PGA
      , Q, C, QGP, QGA
      , PQ
      , PQGP
      , V
      ]

  type Id[A] = PDag.Id[A, V]
}

trait ND4JDsl[V[p, a, b, gp, ga, r] <: NNet.Visitor[p, a, b, gp, ga, r]] extends DagDsl[V] {
  
  type Sigmoid[
    R, D <: Dim2[_, _]
  ] = NNet.Sigmoid[R, D, V]

  type DenseLayer[
    S <: Singleton, R, InR <: XInt, OutR <: XInt, OutC <: XInt
  ] = NNet.DenseLayer[S, R, InR, OutR, OutC, V]

  def sigmoid[
    S, D <: Dim2[_, _]
  ]: Sigmoid[S, D] = new Sigmoid[S, D]()

  def denseLayer[
    S <: Singleton, R, In <: Dim2[_, _], Out <: Dim2[_, _]
  ](
    implicit same: Dim2SameCol[In, Out]
  ): DenseLayer[S, R, same.In, same.OutR, same.OutC] = new DenseLayer[S, R, same.In, same.OutR, same.OutC]()

}


trait Transform[Ctx, D] {
  type Out
  def apply(f: D): Out
}
object Transform {
  type Aux[Ctx, D, O] = Transform[Ctx, D] { type Out = O }
}


object Test {
  trait Visitor[RP, RA, RB, RGP, RGA, Out] extends
    NNet.Visitor[RP, RA, RB, RGP, RGA, Out]
    with PDag.Visitor[RP, RA, RB, RGP, RGA, Out]

  val dsl = new ND4JDsl[Visitor] {}
  import dsl._

  def nnetlayer[R, InR <: XInt, OutR <: XInt, OutC <: XInt] = 
    (denseLayer["s", R, InR x OutC, OutR x OutC] >>> sigmoid) ||
    (denseLayer["t", R, InR x OutC, OutR x OutC] >>> sigmoid)


  implicit def nd4jTrans[RP, RA, RB, RGP, RGA]: ND4JTrans[RP, RA, RB, RGP, RGA] = new ND4JTrans[RP, RA, RB, RGP, RGA] {}

  trait ND4JTrans[RP, RA, RB, RGP, RGA]
    extends Transform[ND4J, PDag[RP, RA, RB, RGP, RGA, Visitor]]
    with Visitor[RP, RA, RB, RGP, RGA, ParametrisedDiff[RP, RA, RB, RGP, RGA]] {
      self =>
    
    type Out = ParametrisedDiff[RP, RA, RB, RGP, RGA]

    def apply(f: PDag[RP, RA, RB, RGP, RGA, Visitor]): ParametrisedDiff[RP, RA, RB, RGP, RGA] = 
      f.accept(this)

    def apply[V[p, a, b, gp, ga, r] <: PDag.Visitor[p, a, b, gp, ga, r]](
      f: PDag.Id[RA, V]
    )(implicit
      ev1: HNil === RP
    , ev2: RA === RB
    , ev3: HNil === RGP
    , ev4: Unit === RGA
    ): ParametrisedDiff[RP, RA, RB, RGP, RGA] = new ParametrisedDiff[RP, RA, RB, RGP, RGA] {
      def apply(pq: RP, a: RA): RB = ev2.coerce(a)

      def gradP(p: RP, a: RA): RGP = ev3.coerce(HNil)
      def gradA(p: RP, a: RA): RGA = ev4.coerce(())
    }

    def apply[X, P, PGP, PGA, Q, QGP, QGA, V[p, a, b, gp, ga, r] <: PDag.Visitor[p, a, b, gp, ga, r]](
      f: PDag.ComposePDag[P, RA, X, PGP, PGA, Q, RB, QGP, QGA, RP, RGP, V]
    )(
      implicit
        ev: (PGA, QGA) === RGA
    ): ParametrisedDiff[RP, RA, RB, RGP, RGA] = ???

    def apply[P, PA, PB, PGP, PGA, Q, QA, QB, QGP, QGA, V[p, a, b, gp, ga, r] <: PDag.Visitor[p, a, b, gp, ga, r]](
      f: PDag.ParallelPDag[P, PA, PB, PGP, PGA, Q, QA, QB, QGP, QGA, RP, RGP, V]
    )(
      implicit
        ev1: (PA, QA) === RA
      , ev2: (PB, QB) === RB
      , ev3: (PGA, QGA) === RGA
    ): ParametrisedDiff[RP, RA, RB, RGP, RGA] = ???

    def apply[
      S, D <: Dim2[_, _]
    , V[p, a, b, gp, ga, r] <: NNet.Visitor[p, a, b, gp, ga, r]
    ](
      sigmoid: NNet.Sigmoid[S, D, V]
    )(implicit
      ev1: HNil === RP
    , ev2: Mat[S, D] === RA
    , ev3: RA === RB
    , ev4: HNil === RGP
    , ev5: Mat[S, D] === RGA
    ): ParametrisedDiff[RP, RA, RB, RGP, RGA] = ???

    def apply[
      S <: Singleton, R, InR <: XInt, OutR <: XInt, OutC <: XInt
    , V[p, a, b, gp, ga, r] <: NNet.Visitor[p, a, b, gp, ga, r]
    ](
      dense: NNet.DenseLayer[S, R, InR, OutR, OutC, V]
    )(implicit
      ev1: (FieldType[S, Mat[R, OutR x InR]] :: HNil) === RP
    , ev2: Mat[R, InR x OutC] === RA
    , ev3: Mat[R, OutR x OutC] === RB
    , ev4: (FieldType[S, Mat[R, InR x OutC]] :: HNil) === RGP
    , ev5: Mat[R, OutR x InR] === RGA
    ): ParametrisedDiff[RP, RA, RB, RGP, RGA] = ???

   def apply[
      P, A, B, GP, GA
    , V[p, a, b, gp, ga, r] <: PDag.Visitor[p, a, b, gp, ga, r]
    ](
      f: PDag.GradPDag[P, A, B, GP, GA, V]
    )(implicit
      ev1: HNil === RP
    , ev2: (P, A) === RA
    , ev3: GP === RB
    , ev4: HNil === RGP
    , ev5: Void === RGA
    ): ParametrisedDiff[RP, RA, RB, RGP, RGA] = ???
    // new ParametrisedDiff[RP, RA, RB, RGP, RGA] {
      // val t: Visitor[P, A, B, GP, GA, ParametrisedDiff[P, A, B, GP, GA]] = nd4jTrans[P, A, B, GP, GA]
      // val dagp = f.dag.accept(t)
      // def apply(rp: RP, ra: RA): RB = {
      //   val (p, a) = ev2.coerce(ra)
      //   ev3.coerce(dagp.gradP(p, a))
      // }
      // def gradP(rp: RP, ra: RA): RGP = ??? //ev3.coerce(HNil)
      // def gradA(rp: RP, ra: RA): RGA = ??? //ev4.coerce(())
    // }

    def apply[
      P, A, B, GP, GA
    , V[p, a, b, gp, ga, r] <: PDag.Visitor[p, a, b, gp, ga, r]
    ](
      f: PDag.GradADag[P, A, B, GP, GA, V]
    )(implicit
      ev1: HNil === RP
    , ev2: (P, A) === RA
    , ev3: GA === RB
    , ev4: HNil === RGP
    , ev5: Void === RGA
    ): ParametrisedDiff[RP, RA, RB, RGP, RGA] = ???
  }

  val net = nnetlayer[Double, 1, 1, 3]

  val f = net.compile[ND4J]


  @inline def implement[P, PA, PB, PGP, PGA, V[p, a, b, gp, ga, r] <: PDag.Visitor[p, a, b, gp, ga, r]](
    idag: PDag[P, PA, PB, PGP, PGA, V]
  ): PDag[P, PA, PB, PGP, PGA, V] = idag

  def update[P, PA, PB, PGP, PGA, V[p, a, b, gp, ga, r] <: PDag.Visitor[p, a, b, gp, ga, r], S, O, OG](
    idag: PDag[P, PA, PB, PGP, PGA, V]
  , costDiff: PDag.Dag[(PB, PB), PB, V]
  , eps: S
  )(implicit
    merger: Merger.Aux[P, HNil, O]
  , mergerg: Merger.Aux[PGP, HNil, OG]
  , merger2: Merger[O, HNil]
  , merger3: Merger[OG, HNil]
  ): PDag.Dag[(P, PA, PB), P, V] = {
    val impl = implement(idag)
    val err = (impl || PDag.Id[PB, V]) >>> costDiff
    // val r: Cmp3 = (Const(eps) || ((GradP(ff) || err) >>> multGBPP())) >>> multSPP()

    // (Id[P] || r) >>> sub()
    ???
  }
}



