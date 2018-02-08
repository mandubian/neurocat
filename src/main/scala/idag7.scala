package neurocat
package idag7

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


sealed trait PDag[P, A, B, GradP, GradA, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]] {

  // the ugly GADT patch
  def compile[Ctx, Out[p, a, b, gp, ga], V2[ctx, o[p, a, b, gp, ga]] <: V[ctx, o]](
    compiler: V2[Ctx, Out]
  )(implicit ev: V2[Ctx, Out] <~< V[Ctx, Out]): Out[P, A, B, GradP, GradA]

}

trait Merger[P, Q]{
  type Out
  def left(p: Out): P
  def right(p: Out): Q
  def apply(p: P, q: Q): Out
} 

object Merger extends Merger2 {

  implicit def hnilMerger[P]: Merger.Aux[HNil, P, P] = new Merger[HNil, P] {
    type Out = P

    def left(pq: P): HNil = HNil
    def right(p: P): P = p
    def apply(hnil: HNil, p: P): P = p
  }
}

trait Merger2 extends Merger3 {

  implicit def hnil2Merger[P]: Merger.Aux[P, HNil, P] = new Merger[P, HNil] {
    type Out = P

    def left(p: P): P = p
    def right(p: P): HNil = HNil
    def apply(p: P, hnil: HNil): P = p
  }

}

trait Merger3 {
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

trait CanMult[A, B, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]] {
  type Out
  def apply(): PDag[HNil, (A, B), Out, HNil, HNil, V]
}

object CanMult {
  type Aux[A, B, C, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]] = CanMult[A, B, V] { type Out = C }
}

trait CanSub[A, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]] {
  def apply(): PDag[HNil, (A, A), A, HNil, HNil, V]
}

trait Norm[A, S] {
  def norm: S
}


trait Zeroed[A] {
  def zero: A
}

object Zeroed {
  def apply[A](implicit zeroed: Zeroed[A]) = zeroed
}


trait Oned[A] {
  def one: A
}

object Oned {
  def apply[A](implicit oned: Oned[A]) = oned
}


trait GradCompose[F, G, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]] {
  type Out
  def apply(): PDag[HNil, (G, F), Out, HNil, HNil, V]
}

object GradCompose {
  type Aux[F, G, FG, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]] = GradCompose[F, G, V] { type Out = FG }
}


trait TypeCompiler[P] {
  type Out
  def compile(p: P): Out
}
object TypeCompiler {
  type Aux[P, O] = TypeCompiler[P] { type Out = O }
}


trait Tuplizer[P, A] {
  type Out

  def tuplize(p: P, a: A): Out
  def untuplize(pa: Out): (P, A)
}

object Tuplizer extends TuplizerImplicits{

  implicit def tupl0[A] = new Tuplizer[HNil, A] {
    type Out = A
    def tuplize(p: HNil, a: A): A = a
    def untuplize(pa: A): (HNil, A) = (HNil, pa)
  }
}

trait TuplizerImplicits {
  type Aux[P, A, O] = Tuplizer[P, A] { type Out = O }

  implicit def tupl1[P, A] = new Tuplizer[P, A] {
    type Out = (P, A)
    def tuplize(p: P, a: A): (P, A) = (p, a)
    def untuplize(pa: (P, A)): (P, A) = pa
  }
}
object PDag {
  
  implicit def mmab[A : MultiplicativeMonoid, B : MultiplicativeMonoid] =
    new MultiplicativeMonoid[(A, B)] {
      def times(x: (A, B), y: (A, B)): (A, B) = (MultiplicativeMonoid[A].times(x._1, y._1), MultiplicativeMonoid[B].times(x._2, y._2))
      def one: (A, B) = (MultiplicativeMonoid[A].one, MultiplicativeMonoid[B].one)
    }

  implicit def amab[A : AdditiveMonoid, B : AdditiveMonoid] =
    new AdditiveMonoid[(A, B)] {
      def plus(x: (A, B), y: (A, B)): (A, B) = (AdditiveMonoid[A].plus(x._1, y._1), AdditiveMonoid[B].plus(x._2, y._2))
      def zero: (A, B) = (AdditiveMonoid[A].zero, AdditiveMonoid[B].zero)
    }

  implicit def tupleZeroed[A : Zeroed, B : Zeroed] =
    new Zeroed[(A, B)] {
      def zero = (Zeroed[A].zero, Zeroed[B].zero)
    }

  implicit def tupleOned[A : Oned, B : Oned] =
    new Oned[(A, B)] {
      def one = (Oned[A].one, Oned[B].one)
    }

  abstract class ProdPDag[
    P, PA, PB, PGP, PGA
  , Q, QB, QGP, QGA
  , PQ
  , PQGP
  , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  ] private[neurocat] (
    val f: PDag[P, PA, PB, PGP, PGA, V]
  , val g: PDag[Q, PA, QB, QGP, QGA, V]
  ) extends PDag[
    PQ, PA, (PB, QB), PQGP, (PGA, QGA), V
  ] {

    def merger: Merger.Aux[P, Q, PQ]
    def mergerg: Merger.Aux[PGP, QGP, PQGP]

    def compile[Ctx, Out[p, a, b, gp, ga], V2[ctx, o[p, a, b, gp, ga]] <: V[ctx, o]](
      compiler: V2[Ctx, Out]
    )(implicit ev: V2[Ctx, Out] <~< V[Ctx, Out]): Out[PQ, PA, (PB, QB), PQGP, (PGA, QGA)] = ??? //compiler(this)

  }


  case class TerminalPDag[
    A: AdditiveMonoid
  , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  ]() extends PDag[
    HNil, A, Unit, HNil, Unit, V
  ] {

    def compile[Ctx, Out[p, a, b, gp, ga], V2[ctx, o[p, a, b, gp, ga]] <: V[ctx, o]](
      compiler: V2[Ctx, Out]
    )(implicit ev: V2[Ctx, Out] <~< V[Ctx, Out]): Out[HNil, A, Unit, HNil, Unit] = ??? //compiler(this)

  }

  case class FstPDag[
    A : Oned, B : Zeroed
  , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  ]() extends PDag[
    HNil, (A, B), A, HNil, A, V
  ] {

    def compile[Ctx, Out[p, a, b, gp, ga], V2[ctx, o[p, a, b, gp, ga]] <: V[ctx, o]](
      compiler: V2[Ctx, Out]
    )(implicit ev: V2[Ctx, Out] <~< V[Ctx, Out]): Out[HNil, (A, B), A, HNil, A] = ??? //compiler(this)

  }

  case class SndPDag[
    A : Zeroed, B : Oned
  , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  ]() extends PDag[
    HNil, (A, B), B, HNil, B, V
  ] {

    def compile[Ctx, Out[p, a, b, gp, ga], V2[ctx, o[p, a, b, gp, ga]] <: V[ctx, o]](
      compiler: V2[Ctx, Out]
    )(implicit ev: V2[Ctx, Out] <~< V[Ctx, Out]): Out[HNil, (A, B), B, HNil, B] = ??? //compiler(this)

  }



  // abstract class ParallelPDag[
  //   P, PA, PB, PGP, PGA
  // , Q, QA, QB, QGP, QGA
  // , PQ
  // , PQGP
  // , PQGA
  // , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  // ] private[neurocat] (
  //   val f: PDag[P, PA, PB, PGP, PGA, V]
  // , val g: PDag[Q, QA, QB, QGP, QGA, V]
  // ) extends PDag[
  //   PQ, (PA, QA), (PB, QB), PQGP, PQGA, V
  // ] {

  //   def merger: Merger.Aux[P, Q, PQ]
  //   def mergerg: Merger.Aux[PGP, QGP, PQGP]
  //   def mergera: Merger.Aux[PGA, QGA, PQGA]

  //   def compile[Ctx, Out[p, a, b, gp, ga], V2[ctx, o[p, a, b, gp, ga]] <: V[ctx, o]](
  //     compiler: V2[Ctx, Out]
  //   )(implicit ev: V2[Ctx, Out] <~< V[Ctx, Out]): Out[PQ, (PA, QA), (PB, QB), PQGP, PQGA] = compiler(this)

  // }


  abstract class ComposePDag[
    P, A, B, PGP, PGA
  , Q, C, QGP, QGA
  , PQ
  , PQGP
  , PQGA
  , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  ] private[neurocat] (
    val g: PDag[Q, B, C, QGP, QGA, V]
  , val f: PDag[P, A, B, PGP, PGA, V]
  ) extends PDag[PQ, A, C, PQGP, PQGA, V] {

    def merger: Merger.Aux[P, Q, PQ]
    def mergerg: Merger.Aux[PGP, QGP, PQGP]
    def gcomp: GradCompose.Aux[PGA, QGA, PQGA, V]

    def compile[Ctx, Out[p, a, b, gp, ga], V2[ctx, o[p, a, b, gp, ga]] <: V[ctx, o]](
      compiler: V2[Ctx, Out]
    )(implicit ev: V2[Ctx, Out] <~< V[Ctx, Out]): Out[PQ, A, C, PQGP, PQGA] = compiler(this)

  }


  case class Id[A : Oned, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]]()
  extends PDag[HNil, A, A, HNil, A, V] {

    def compile[Ctx, Out[p, a, b, gp, ga], V2[ctx, o[p, a, b, gp, ga]] <: V[ctx, o]](
      compiler: V2[Ctx, Out]
    )(implicit ev: V2[Ctx, Out] <~< V[Ctx, Out]): Out[HNil, A, A, HNil, A] = compiler(this)

  }

  case class ComonoidalSplit[A : Oned : Comonoid, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]]()
  extends PDag[HNil, A, (A, A), HNil, (A, A), V] {

    def compile[Ctx, Out[p, a, b, gp, ga], V2[ctx, o[p, a, b, gp, ga]] <: V[ctx, o]](
      compiler: V2[Ctx, Out]
    )(implicit ev: V2[Ctx, Out] <~< V[Ctx, Out]): Out[HNil, A, (A, A), HNil, (A, A)] = compiler(this)

  }

  case class MonoidalJoin[A : Oned : AdditiveMonoid, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]]()
  extends PDag[HNil, (A, A), A, HNil, A, V] {

    def compile[Ctx, Out[p, a, b, gp, ga], V2[ctx, o[p, a, b, gp, ga]] <: V[ctx, o]](
      compiler: V2[Ctx, Out]
    )(implicit ev: V2[Ctx, Out] <~< V[Ctx, Out]): Out[HNil, (A, A), A, HNil, A] = compiler(this)

  }  

  // case class AssocRR[P, PA, A, B, C, GP, GA, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]](
  //   val dagg: PDag[P, PA, ((A, B), C), GP, GA, V]
  // )
  // extends PDag[P, PA, (A, (B, C)), GP, GA, V] {

  //   def compile[Ctx, Out[p, a, b, gp, ga], V2[ctx, o[p, a, b, gp, ga]] <: V[ctx, o]](
  //     compiler: V2[Ctx, Out]
  //   )(implicit ev: V2[Ctx, Out] <~< V[Ctx, Out]): Out[P, PA, (A, (B, C)), GP, GA] = compiler(this)

  // }

  // case class AssocRL[P, PA, A, B, C, GP, GA, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]](
  //   val dag: PDag[P, PA, (A, (B, C)), GP, GA, V]
  // )
  // extends PDag[P, PA, ((A, B), C), GP, GA, V] {

  //   def compile[Ctx, Out[p, a, b, gp, ga], V2[ctx, o[p, a, b, gp, ga]] <: V[ctx, o]](
  //     compiler: V2[Ctx, Out]
  //   )(implicit ev: V2[Ctx, Out] <~< V[Ctx, Out]): Out[P, PA, ((A, B), C), GP, GA] = compiler(this)

  // }

  //  case class AssocLR[P, A, B, C, PB, GP, GA, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]](
  //   val dag: PDag[P, ((A, B), C), PB, GP, GA, V]
  // )
  // extends PDag[P, (A, (B, C)), PB, GP, GA, V] {

  //   def compile[Ctx, Out[p, a, b, gp, ga], V2[ctx, o[p, a, b, gp, ga]] <: V[ctx, o]](
  //     compiler: V2[Ctx, Out]
  //   )(implicit ev: V2[Ctx, Out] <~< V[Ctx, Out]): Out[P, (A, (B, C)), PB, GP, GA] = compiler(this)

  // }

  // case class AssocLL[P, A, B, C, PB, GP, GA, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]](
  //   val dag: PDag[P, (A, (B, C)), PB, GP, GA, V]
  // )
  // extends PDag[P, ((A, B), C), PB, GP, GA, V] {

  //   def compile[Ctx, Out[p, a, b, gp, ga], V2[ctx, o[p, a, b, gp, ga]] <: V[ctx, o]](
  //     compiler: V2[Ctx, Out]
  //   )(implicit ev: V2[Ctx, Out] <~< V[Ctx, Out]): Out[P, ((A, B), C), PB, GP, GA] = compiler(this)

  // }




  trait ExtPDag[
    P, A, B, GP, GA
  , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  ] extends PDag[P, A, B, GP, GA, V]


  abstract class Dag[
    P, A, B, PGP, PGA
  , PA
  , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  ] private[neurocat](val dag: PDag[P, A, B, PGP, PGA, V]) extends PDag[HNil, PA, B, HNil, HNil, V]  {

    def tuplizer: Tuplizer.Aux[P, A, PA]

    def compile[Ctx, Out[p, a, b, gp, ga], V2[ctx, o[p, a, b, gp, ga]] <: V[ctx, o]](
      compiler: V2[Ctx, Out]
    )(implicit ev: V2[Ctx, Out] <~< V[Ctx, Out]): Out[HNil, PA, B, HNil, HNil] = compiler(this)

  }

  case class GradPDag[
    P, A, B, PGP, PGA
  , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  ](dag: PDag[P, A, B, PGP, PGA, V]) extends PDag[HNil, (P, A), PGP, HNil, HNil, V]  {

    def compile[Ctx, Out[p, a, b, gp, ga], V2[ctx, o[p, a, b, gp, ga]] <: V[ctx, o]](
      compiler: V2[Ctx, Out]
    )(implicit ev: V2[Ctx, Out] <~< V[Ctx, Out]): Out[HNil, (P, A), PGP, HNil, HNil] = compiler(this)

  }

  case class GradADag[
    P, A, B, PGP, PGA
  , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  ](dag: PDag[P, A, B, PGP, PGA, V]) extends PDag[HNil, (P, A), PGA, HNil, HNil, V] {

    def compile[Ctx, Out[p, a, b, gp, ga], V2[ctx, o[p, a, b, gp, ga]] <: V[ctx, o]](
      compiler: V2[Ctx, Out]
    )(implicit ev: V2[Ctx, Out] <~< V[Ctx, Out]): Out[HNil, (P, A), PGA, HNil, HNil] = compiler(this)

  }

  final case class Const[
    S
  , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  ](value: S) extends PDag[HNil, Unit, S, HNil, HNil, V] {

    def compile[Ctx, Out[p, a, b, gp, ga], V2[ctx, o[p, a, b, gp, ga]] <: V[ctx, o]](
      compiler: V2[Ctx, Out]
    )(implicit ev: V2[Ctx, Out] <~< V[Ctx, Out]): Out[HNil, Unit, S, HNil, HNil] = compiler(this)

  }

  trait Compiler[Ctx, Out[p, a, b, gp, ga]] {
    self =>

    def apply[
      P, PA, PB, PGP, PGA
    , Q, QB, QGP, QGA
    , PQ
    , PQGP
    , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
    ](par: ProdPDag[
      P, PA, PB, PGP, PGA
    , Q, QB, QGP, QGA
    , PQ
    , PQGP
    , V
    ])(
      implicit
        ev: self.type <~< V[Ctx, Out]
      , 
    ): Out[PQ, PA, (PB, QB), PQGP, (PGA, QGA)]


    def apply[
      P, A: Oned, B: Zeroed, PGP, PGA
    , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
    ](par: FstPDag[
      A, B
    , V
    ])(
      implicit ev: self.type <~< V[Ctx, Out]
    ): Out[HNil, (A, B), A, HNil, (A, B)]


    def apply[
      P, A: Zeroed, B: Oned, PGP, PGA
    , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
    ](par: SndPDag[
      A, B
    , V
    ])(
      implicit ev: self.type <~< V[Ctx, Out]
    ): Out[HNil, (A, B), B, HNil, (A, B)]

    // def apply[
    //   P, PA, PB, PGP, PGA
    // , Q, QA, QB, QGP, QGA
    // , PQ
    // , PQGP
    // , PQGA
    // , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
    // ](par: ParallelPDag[
    //   P, PA, PB, PGP, PGA
    // , Q, QA, QB, QGP, QGA
    // , PQ
    // , PQGP
    // , PQGA
    // , V
    // ])(
    //   implicit ev: self.type <~< V[Ctx, Out]
    // ): Out[PQ, (PA, QA), (PB, QB), PQGP, PQGA]

    def apply[
      P, A, B, PGP, PGA
    , Q, C, QGP, QGA
    , PQ
    , PQGP
    , PQGA
    , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
    ](par: ComposePDag[
      P, A, B, PGP, PGA
    , Q, C, QGP, QGA
    , PQ
    , PQGP
    , PQGA
    , V
    ])(
      implicit ev: self.type <~< V[Ctx, Out]
    ): Out[PQ, A, C, PQGP, PQGA]

    def apply[A : Oned, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]](
      f: Id[A, V]
    )(
      implicit ev: self.type <~< V[Ctx, Out]
    ): Out[HNil, A, A, HNil, A]

    def apply[A : Oned : Comonoid, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]](
      f: ComonoidalSplit[A, V]
    )(
      implicit ev: self.type <~< V[Ctx, Out]
    ): Out[HNil, A, (A, A), HNil, (A, A)]

    def apply[A : Oned : AdditiveMonoid, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]](
      f: MonoidalJoin[A, V]
    )(
      implicit ev: self.type <~< V[Ctx, Out]
    ): Out[HNil, (A, A), A, HNil, A]

    def apply[
      P, A, B, GP, GA, PA
    , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
    ](
      f: PDag.Dag[P, A, B, GP, GA, PA, V]
    )(
      implicit ev: self.type <~< V[Ctx, Out]
    ): Out[HNil, PA, B, HNil, HNil]


    def apply[
      P, A, B, GP, GA
    , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
    ](
      f: PDag.GradPDag[P, A, B, GP, GA, V]
    )(
      implicit ev: self.type <~< V[Ctx, Out]
    ): Out[HNil, (P, A), GP, HNil, HNil]


    def apply[
      P, A, B, GP, GA
    , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
    ](
      f: PDag.GradADag[P, A, B, GP, GA, V]
    )(
      implicit ev: self.type <~< V[Ctx, Out]
    ): Out[HNil, (P, A), GA, HNil, HNil]


    def apply[
      S
    , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
    ](
      f: PDag.Const[S, V]
    )(
      implicit ev: self.type <~< V[Ctx, Out]
    ): Out[HNil, Unit, S, HNil, HNil]

    // def apply[
    //   P, PA, A, B, C, GP, GA, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
    // ](
    //   f: AssocRR[P, PA, A, B, C, GP, GA, V]
    // )(
    //   implicit ev: self.type <~< V[Ctx, Out]
    // ): Out[P, PA, (A, (B, C)), GP, GA]

    // def apply[
    //   P, PA, A, B, C, GP, GA, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
    // ](
    //   f: AssocRL[P, PA, A, B, C, GP, GA, V]
    // )(
    //   implicit ev: self.type <~< V[Ctx, Out]
    // ): Out[P, PA, ((A, B), C), GP, GA]

    // def apply[
    //   P, A, B, C, PB, GP, GA, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
    // ](
    //   f: AssocLR[P, A, B, C, PB, GP, GA, V]
    // )(
    //   implicit ev: self.type <~< V[Ctx, Out]
    // ): Out[P, (A, (B, C)), PB, GP, GA]

    // def apply[
    //   P, A, B, C, PB, GP, GA, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
    // ](
    //   f: AssocLL[P, A, B, C, PB, GP, GA, V]
    // )(
    //   implicit ev: self.type <~< V[Ctx, Out]
    // ): Out[P, ((A, B), C), PB, GP, GA]

  }

  // Partial derivative along (A, B) is (A.1, B.0)...
  def fst[
    A: Oned, B: Zeroed
  , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  ]: PDag[HNil, (A, B), A, HNil, A, V] = FstPDag()

  def snd[
    A: Zeroed, B: Oned
  , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  ]: PDag[HNil, (A, B), B, HNil, B, V] = SndPDag()

  def prod[
    P, PA, PB, PGP, PGA
  , Q, QB, QGP, QGA
  , PQ
  , PQGP
  , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  ](f: PDag[P, PA, PB, PGP, PGA, V], g: PDag[Q, PA, QB, QGP, QGA, V])(
    implicit merger0: Merger[P, Q], mergerg0: Merger[PGP, QGP]
  ): PDag[merger0.Out, PA, (PB, QB), mergerg0.Out, (PGA, QGA), V] = new ProdPDag[
        P, PA, PB, PGP, PGA
      , Q, QB, QGP, QGA
      , merger0.Out
      , mergerg0.Out
      , V
      ](f, g) {
        def merger = merger0
        def mergerg = mergerg0
      }


  def compose[
    P, A, B, PGP, PGA
  , Q, C, QGP, QGA
  , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  ](g: PDag[Q, B, C, QGP, QGA, V], f: PDag[P, A, B, PGP, PGA, V])(
    implicit merger0: Merger[P, Q], mergerg0: Merger[PGP, QGP], gcomp0: GradCompose[PGA, QGA, V]
  ): PDag[merger0.Out, A, C, mergerg0.Out, gcomp0.Out, V] = new ComposePDag[
        P, A, B, PGP, PGA
      , Q, C, QGP, QGA
      , merger0.Out, mergerg0.Out, gcomp0.Out, V](g, f) {
        def merger = merger0
        def mergerg = mergerg0
        def gcomp = gcomp0
      }

  def par[
    P, PA: Oned : Zeroed, PB, PGP, PGA
  , Q, QA: Zeroed : Oned, QB, QGP, QGA
  , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  ](f: PDag[P, PA, PB, PGP, PGA, V], g: PDag[Q, QA, QB, QGP, QGA, V])(
    implicit merger0: Merger[P, Q], mergerg0: Merger[PGP, QGP]
    , gcompp: GradCompose[PA, PGA, V]
    , gcompq: GradCompose[QA, QGA, V]
  ): PDag[merger0.Out, (PA, QA), (PB, QB), mergerg0.Out, (gcompp.Out, gcompq.Out), V] =
    prod(
      compose(f, fst[PA, QA, V])
    , compose(g, snd[PA, QA, V])
    )


  def id[
    A : Oned
  , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  ] = PDag.Id[A, V]

  def diag[
    A : Oned
  , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  ]: PDag[HNil, A, (A, A), HNil, (A, A), V] =
    prod(id[A, V], id[A, V])

  def assocR[
    A : Zeroed : Oned, B : Oned : Zeroed, C : Oned : Zeroed
  , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  ](implicit
    gca: GradCompose.Aux[(A, B), A, A, V]
  , gcb: GradCompose.Aux[(A, B), B, B, V]
  ): PDag[HNil, ((A, B), C), (A, (B, C)), HNil, (A, (B, C)), V] = {
    val pa = compose(fst[A, B, V], fst[(A, B), C, V])
    val pb = compose(snd[A, B, V], fst[(A, B), C, V])
    val pc = snd[(A, B), C, V]
    prod(pa, prod(pb, pc))
  }

  def assocL[
    A : Zeroed : Oned, B : Oned : Zeroed, C : Oned : Zeroed
  , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  ](implicit
    gca: GradCompose.Aux[(B, C), B, B, V]
  , gcb: GradCompose.Aux[(B, C), C, C, V]
  ): PDag[HNil, (A, (B, C)), ((A, B), C), HNil, ((A, B), C), V] = {
    val pb = compose(fst[B, C, V], snd[A, (B, C), V])
    val pc = compose(snd[B, C, V], snd[A, (B, C), V])
    val pa = fst[A, (B, C), V]
    prod(prod(pa, pb), pc)
  }

  def split[
    A : Oned : Comonoid
  , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  ]: PDag[HNil, A, (A, A), HNil, (A, A), V] = PDag.ComonoidalSplit[A, V]()

  def dagify[
    P, A, B, PGP, PGA
  , PA
  , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  ](pdag: PDag[P, A, B, PGP, PGA, V])(
    implicit tuplizer0: Tuplizer[P, A]
  ): PDag[HNil, tuplizer0.Out, B, HNil, HNil, V] = new PDag.Dag[P, A, B, PGP, PGA, tuplizer0.Out, V](pdag)  {

    def tuplizer = tuplizer0

  }

  def flip[
    A : Oned : Zeroed, B : Oned : Zeroed
  , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  ]: PDag[HNil, (A, B), (B, A), HNil, (B, A), V] =
    prod(snd[A, B, V], fst[A, B, V])

  implicit class Ops[
    P, PA, PB, PGP, PGA
  , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  ](
    idag: PDag[P, PA, PB, PGP, PGA, V]
  ) {
    def ||[Q, QA, QB, QGP, QGA](other: PDag[Q, QA, QB, QGP, QGA, V])(
      implicit
      merger: Merger[P, Q]
    , mergerg: Merger[PGP, QGP]
    , mmpa: Oned[PA]
    , ampa: Zeroed[PA]
    , mmqa: Oned[QA]
    , amqa: Zeroed[QA]
    , cm: GradCompose[PA, PGA, V]
    , cm2: GradCompose[QA, QGA, V]
    ): PDag[merger.Out, (PA, QA), (PB, QB), mergerg.Out, (cm.Out, cm2.Out), V] = par(idag, other)

    def **[Q, QB, QGP, QGA](other: PDag[Q, PA, QB, QGP, QGA, V])(
      implicit merger: Merger[P, Q], mergerg: Merger[PGP, QGP]
    ): PDag[merger.Out, PA, (PB, QB), mergerg.Out, (PGA, QGA), V] = prod(idag, other)

    def >>>[Q, QC, QGP, QGA](other: PDag[Q, PB, QC, QGP, QGA, V])(
      implicit merger: Merger[P, Q], mergerg: Merger[PGP, QGP], gcompa: GradCompose[PGA, QGA, V]
    ): PDag[merger.Out, PA, QC, mergerg.Out, gcompa.Out, V] = compose(other, idag)

  }
}



object NNet {

  case class MatMult[R, A <: XInt, B <: XInt, C <: XInt, V[ctx, o[p, a, b, gp, ga]] <: NNet.Compiler[ctx, o]]()
  extends PDag.ExtPDag[HNil, (Mat[R, A x B], Mat[R, B x C]), Mat[R, A x C], HNil, HNil, V] {
    def compile[Ctx, Out[p, a, b, gp, ga], V2[ctx, o[p, a, b, gp, ga]] <: V[ctx, o]](
      compiler: V2[Ctx, Out]
    )(implicit ev: V2[Ctx, Out] <~< V[Ctx, Out]): Out[HNil, (Mat[R, A x B], Mat[R, B x C]), Mat[R, A x C], HNil, HNil] = ??? //compiler(this)
  }

  // the derivative of a V => V function keeping same dimensions has size of the transpose of V
  // (jacobian can be reduced to its diagonal)
  case class Sigmoid[S, R <: XInt, V[ctx, o[p, a, b, gp, ga]] <: NNet.Compiler[ctx, o]]()
  extends PDag.ExtPDag[HNil, Mat[S, R x 1], Mat[S, R x 1], HNil, Mat[S, R x R], V] {

    def compile[Ctx, Out[p, a, b, gp, ga], V2[ctx, o[p, a, b, gp, ga]] <: V[ctx, o]](
      compiler: V2[Ctx, Out]
    )(implicit ev: V2[Ctx, Out] <~< V[Ctx, Out]): Out[HNil, Mat[S, R x 1], Mat[S, R x 1], HNil, Mat[S, R x R]] = compiler(this)
  }


  class DenseLayer[
    S <: Singleton, R, InR <: XInt, OutR <: XInt, OutC <: XInt
  , V[ctx, o[p, a, b, gp, ga]] <: NNet.Compiler[ctx, o]
  ]()
    extends PDag.ExtPDag[
      FieldType[S, Mat[R, OutR x InR]] :: HNil
    , Mat[R, InR x OutC]
    , Mat[R, OutR x OutC]
    , FieldType[S, Mat[R, InR x OutC]] :: HNil
    , Mat[R, OutR x InR]
    , V
    ] {
      def compile[Ctx, Out[p, a, b, gp, ga], V2[ctx, o[p, a, b, gp, ga]] <: V[ctx, o]](
        compiler: V2[Ctx, Out]
      )(implicit ev: V2[Ctx, Out] <~< V[Ctx, Out]): Out[
        FieldType[S, Mat[R, OutR x InR]] :: HNil
      , Mat[R, InR x OutC]
      , Mat[R, OutR x OutC]
      , FieldType[S, Mat[R, InR x OutC]] :: HNil
      , Mat[R, OutR x InR]
      ] = compiler(this)
    }


  trait Compiler[Ctx, Out[p, a, b, gp, ga]] extends PDag.Compiler[Ctx, Out] {
    self =>

    def apply[S, R <: XInt,  V[ctx, o[p, a, b, gp, ga]] <: NNet.Compiler[ctx, o]](
      sigmoid: Sigmoid[S, R, V]
    )(implicit ev: self.type <~< V[Ctx, Out]): Out[HNil, Mat[S, R x 1], Mat[S, R x 1], HNil, Mat[S, R x R]]

    def apply[
      S <: Singleton, R, InR <: XInt, OutR <: XInt, OutC <: XInt
    , V[ctx, o[p, a, b, gp, ga]] <: NNet.Compiler[ctx, o]
    ](
      dense: DenseLayer[S, R, InR, OutR, OutC, V]
    )(implicit ev: self.type <~< V[Ctx, Out]): Out[
      FieldType[S, Mat[R, OutR x InR]] :: HNil, Mat[R, InR x OutC]
    , Mat[R, OutR x OutC]
    , FieldType[S, Mat[R, InR x OutC]] :: HNil
    , Mat[R, OutR x InR]
    ]
  }

}




trait ParametrisedFunction[P, A, B] extends Function2[P, A, B]

trait ParametrisedDiff[P, A, B, GradP, GradA] extends ParametrisedFunction[P, A, B] {
  def gradP(p: P, a: A): GradP
  def gradA(p: P, a: A): GradA
}


trait DagDsl[V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]] {
  type ProdPDag[
    P, PA, PB, PGP, PGA
  , Q, QB, QGP, QGA
  , PQ
  , PQGP
  ] = PDag.ProdPDag[
        P, PA, PB, PGP, PGA
      , Q, QB, QGP, QGA
      , PQ
      , PQGP
      , V
      ]

  type ComposePDag[
    P, A, B, PGP, PGA
  , Q, C, QGP, QGA
  , PQ
  , PQGP
  , PQGA
  ] = PDag.ComposePDag[
        P, A, B, PGP, PGA
      , Q, C, QGP, QGA
      , PQ
      , PQGP
      , PQGA
      , V
      ]

  type Id[A] = PDag.Id[A, V]
}

trait ND4JDsl[V[ctx, o[p, a, b, gp, ga]] <: NNet.Compiler[ctx, o]] extends DagDsl[V] {
  
  type Sigmoid[
    S, R <: XInt
  ] = NNet.Sigmoid[S, R, V]

  type DenseLayer[
    S <: Singleton, R, InR <: XInt, OutR <: XInt, OutC <: XInt
  ] = NNet.DenseLayer[S, R, InR, OutR, OutC, V]

  def sigmoid[
    S, X <: XInt
  ]: Sigmoid[S, X] = new Sigmoid[S, X]()

  def denseLayer[
    S <: Singleton, R, In <: Dim2[_, _], Out <: Dim2[_, _]
  ](
    implicit same: Dim2SameCol[In, Out]
  ): DenseLayer[S, R, same.In, same.OutR, same.OutC] = new DenseLayer[S, R, same.In, same.OutR, same.OutC]()

}


sealed trait Mat[S, D <: Dim2[_, _]]

case class Matrix[S, D <: Dim2[_, _]]() extends Mat[S, D]
case class Identity[S, R <: XInt]() extends Mat[S, R x R]
case class One[S, D <: Dim2[_, _]]() extends Mat[S, D]
case class Zero[S, D <: Dim2[_, _]]() extends Mat[S, D]
case class Diag[S, R <: XInt]() extends Mat[S, R x R]



trait ND4J

object ND4J extends ND4J {

  // abstract class ND4JParametrisedDiff[P, A, B, GP, GA] {
  //   type P0
  //   type A0
  //   type B0
  //   type GP0
  //   type GA0

  //   def convertP: TypeConvert.Aux[P, P0]
  //   def convertA: TypeConvert.Aux[A, A0]
  //   def convertB: TypeConvert.Aux[B, B0]
  //   def convertGP: TypeConvert.Aux[GP, GP0]
  //   def convertGA: TypeConvert.Aux[GA, GA0]

  //   def apply(): ParametrisedDiff[P0, A0, B0, GP0, GA0] 
  // }
}


object Test {

  trait Compiler[Ctx, Out[p, a, b, gp, ga]] extends
    PDag.Compiler[Ctx, Out] with NNet.Compiler[Ctx, Out]


  val dsl = new ND4JDsl[Compiler] {}
  import dsl._


  implicit def matCanMult[
    R, A <: XInt, B <: XInt, C <: XInt
  , V[ctx, o[p, a, b, gp, ga]] <: NNet.Compiler[ctx, o]
  ] = new CanMult[Mat[R, A x B], Mat[R, B x C], V] {
    type Out = Mat[R, A x C]
    def apply() = NNet.MatMult[R, A, B, C, V]() 
  }

  implicit def matGradCompose[
    R, A <: XInt, B <: XInt, C <: XInt
  , V[ctx, o[p, a, b, gp, ga]] <: NNet.Compiler[ctx, o]
  ] = new GradCompose[Mat[R, B x C], Mat[R, A x B], V] {
    type Out = Mat[R, A x C]
    def apply(): PDag[HNil, (Mat[R, A x B], Mat[R, B x C]), Mat[R, A x C], HNil, HNil, V] = 
      NNet.MatMult[R, A, B, C, V]()
  }

  implicit def matZeroed[
    R, A <: XInt, B <: XInt
  ] = new Zeroed[Mat[R, A x B]] {
    def zero = Zero[R, A x B]()
  }

  implicit def matOned[
    R, A <: XInt
  ] = new Oned[Mat[R, A x 1]] {
    def one = One[R, A x 1]()
  }

  def nnetlayer[S, InR <: XInt, OutR <: XInt] = 
    (denseLayer["s", S, InR x 1, OutR x 1] >>> sigmoid) ||
      (denseLayer["t", S, InR x 1, OutR x 1] >>> sigmoid)


  // implicit val ND4JTrans = new Compiler[ND4J, ND4J.ND4JParametrisedDiff] { self =>
    
  //   def apply[
  //     P, PA, PB, PGP, PGA
  //   , Q, QA, QB, QGP, QGA
  //   , PQ
  //   , PQGP
  //   , PQGA
  //   , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  //   ](par: PDag.ParallelPDag[
  //     P, PA, PB, PGP, PGA
  //   , Q, QA, QB, QGP, QGA
  //   , PQ
  //   , PQGP
  //   , PQGA
  //   , V
  //   ])(implicit ev: self.type <~< V[ND4J, ParametrisedDiff]): ParametrisedDiff[PQ, (PA, QA), (PB, QB), PQGP, PQGA] = {
  //     new ParametrisedDiff[PQ, (PA, QA), (PB, QB), PQGP, PQGA] {
  //       val pf = par.f.compile(ev.coerce(self))
  //       val pg = par.g.compile(ev.coerce(self))

  //       def apply(pq: PQ, ab: (PA, QA)): (PB, QB) = {
  //         ( pf(par.merger.left(pq), ab._1)
  //         , pg(par.merger.right(pq), ab._2)
  //         )
  //       }

  //       def gradP(pq: PQ, ab: (PA, QA)): PQGP = {
  //         par.mergerg(pf.gradP(par.merger.left(pq), ab._1), pg.gradP(par.merger.right(pq), ab._2))
  //       }

  //       def gradA(pq: PQ, ab: (PA, QA)): PQGA = {
  //         par.mergera(pf.gradA(par.merger.left(pq), ab._1), pg.gradA(par.merger.right(pq), ab._2))
  //       }
  //     }
  //   }

  //   def apply[
  //     P, A, B, PGP, PGA
  //   , Q, C, QGP, QGA
  //   , PQ
  //   , PQGP
  //   , PQGA
  //   , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  //   ](cmp: PDag.ComposePDag[
  //     P, A, B, PGP, PGA
  //   , Q, C, QGP, QGA
  //   , PQ
  //   , PQGP
  //   , PQGA
  //   , V
  //   ])(implicit ev: self.type <~< V[ND4J, ParametrisedDiff]): ParametrisedDiff[PQ, A, C, PQGP, PQGA] = {
  //     new ParametrisedDiff[PQ, A, C, PQGP, PQGA] {
  //       val pf = cmp.f.compile(ev.coerce(self))
  //       val pg = cmp.g.compile(ev.coerce(self))
  //       val multa = cmp.multa().compile(ev.coerce(self))

  //       def apply(pq: PQ, a: A): C = {
  //         pg(cmp.merger.right(pq), pf(cmp.merger.left(pq), a))
  //       }

  //       def gradP(pq: PQ, a: A): PQGP = {
  //         cmp.mergerg(pf.gradP(cmp.merger.left(pq), a), pg.gradP(cmp.merger.right(pq), pf(cmp.merger.left(pq), a)))
  //       }

  //       def gradA(pq: PQ, a: A): PQGA = {
  //         multa(
  //           HNil
  //         , pf.gradA(cmp.merger.left(pq), a) -> pg.gradA(cmp.merger.right(pq), pf(cmp.merger.left(pq), a))
  //         )
  //       }
  //     }
  //   }

    // def apply[A : MultiplicativeMonoid, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]](
    //   f: PDag.Id[A, V]
    // )(
    //   implicit
    //     ev: self.type <~< V[ND4J, ND4J.ND4JParametrisedDiff]
    //   , convertP0: ND4J.TypeConvert[HNil]
    //   , convertA0: ND4J.TypeConvert[A]
    // ): ND4J.ND4JParametrisedDiff[HNil, A, A, HNil, A] = new ND4J.ND4JParametrisedDiff[HNil, A, A, HNil, A] {
    //   type P0 = convertP0.Out
    //   type A0 = convertA0.Out
    //   type B0 = convertA0.Out
    //   type GP0 = convertP0.Out
    //   type GA0 = convertA0.Out

    //   val convertP = convertP0
    //   val convertA = convertA0
    //   val convertB = convertA0
    //   val convertGP = convertP0
    //   val convertGA = convertA0

    //   def apply() = new ParametrisedDiff[convertP0.Out, convertA0.Out, convertA0.Out, convertP0.Out, convertA0.Out] {
    //     def apply(pq: HNil, a: A): A = a

    //     def gradP(p: HNil, a: A): HNil = HNil
    //     def gradA(p: HNil, a: A): A = MultiplicativeMonoid[A].one
    //   }
    // }


  //   def apply[A : MultiplicativeMonoid : Comonoid, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]](
  //     f: PDag.ComonoidalSplit[A, V]
  //   )(
  //     implicit ev: self.type <~< V[ND4J, ParametrisedDiff]
  //   ): ParametrisedDiff[HNil, A, (A, A), HNil, A] = new ParametrisedDiff[HNil, A, (A, A), HNil, A] {
  //     def apply(pq: HNil, a: A): (A, A) = Comonoid[A].Comonoidalsplit(a)

  //     def gradP(p: HNil, a: A): HNil = HNil
  //     def gradA(p: HNil, a: A): A = MultiplicativeMonoid[A].one
  //   }

  //   def apply[A : MultiplicativeMonoid : AdditiveMonoid, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]](
  //     f: PDag.MonoidalJoin[A, V]
  //   )(
  //     implicit ev: self.type <~< V[ND4J, ParametrisedDiff]
  //   ): ParametrisedDiff[HNil, (A, A), A, HNil, (A, A)] = new ParametrisedDiff[HNil, (A, A), A, HNil, (A, A)] {
  //     def apply(pq: HNil, a: (A, A)): A = AdditiveMonoid[A].plus(a._1, a._2)

  //     def gradP(p: HNil, a: (A, A)): HNil = HNil
  //     def gradA(p: HNil, a: (A, A)): (A, A) = (MultiplicativeMonoid[A].one, MultiplicativeMonoid[A].one)
  //   }

  //   def apply[
  //     P, A, B, GP, GA, PA
  //   , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  //   ](
  //     f: PDag.Dag[P, A, B, GP, GA, PA, V]
  //   )(
  //     implicit ev: self.type <~< V[ND4J, ParametrisedDiff]
  //   ): ParametrisedDiff[HNil, PA, B, HNil, HNil] =
  //     new ParametrisedDiff[HNil, PA, B, HNil, HNil] {
  //       val pf = f.dag.compile(ev.coerce(self))

  //       def apply(pq: HNil, pa: PA): B = {
  //         val (p, a) = f.tuplizer.untuplize(pa)
  //         pf(p, a)
  //       }

  //       def gradP(p: HNil, pa: PA): HNil = HNil
  //       def gradA(p: HNil, pa: PA): HNil = HNil

  //     }

  //   def apply[
  //     P, A, B, GP, GA
  //   , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  //   ](
  //     f: PDag.GradPDag[P, A, B, GP, GA, V]
  //   )(
  //     implicit ev: self.type <~< V[ND4J, ParametrisedDiff]
  //   ): ParametrisedDiff[HNil, (P, A), GP, HNil, HNil] =
  //     new ParametrisedDiff[HNil, (P, A), GP, HNil, HNil] {
  //       val pf = f.dag.compile(ev.coerce(self))

  //       def apply(pq: HNil, pa: (P, A)): GP = pf.gradP(pa._1, pa._2)

  //       def gradP(p: HNil, pa: (P, A)): HNil = HNil
  //       def gradA(p: HNil, pa: (P, A)): HNil = HNil

  //     }

  //   def apply[
  //     P, A, B, GP, GA
  //   , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  //   ](
  //     f: PDag.GradADag[P, A, B, GP, GA, V]
  //   )(
  //     implicit ev: self.type <~< V[ND4J, ParametrisedDiff]
  //   ): ParametrisedDiff[HNil, (P, A), GA, HNil, HNil] =
  //     new ParametrisedDiff[HNil, (P, A), GA, HNil, HNil] {
  //       val pf = f.dag.compile(ev.coerce(self))

  //       def apply(pq: HNil, pa: (P, A)): GA = pf.gradA(pa._1, pa._2)

  //       def gradP(p: HNil, pa: (P, A)): HNil = HNil
  //       def gradA(p: HNil, pa: (P, A)): HNil = HNil

  //     }

  //   def apply[
  //     S
  //   , V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  //   ](
  //     f: PDag.Const[S, V]
  //   )(
  //     implicit ev: self.type <~< V[ND4J, ParametrisedDiff]
  //   ): ParametrisedDiff[HNil, Unit, S, HNil, HNil] = 
  //     new ParametrisedDiff[HNil, Unit, S, HNil, HNil] {
  //       def apply(pq: HNil, pa: Unit): S = f.value

  //       def gradP(p: HNil, pa: Unit): HNil = HNil
  //       def gradA(p: HNil, pa: Unit): HNil = HNil

  //     }

  //   def apply[
  //     P, PA, A, B, C, GP, GA, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  //   ](
  //     f: PDag.AssocRR[P, PA, A, B, C, GP, GA, V]
  //   )(
  //     implicit ev: self.type <~< V[ND4J, ParametrisedDiff]
  //   ): ParametrisedDiff[P, PA, (A, (B, C)), GP, GA] = ???

  //   def apply[
  //     P, PA, A, B, C, GP, GA, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  //   ](
  //     f: PDag.AssocRL[P, PA, A, B, C, GP, GA, V]
  //   )(
  //     implicit ev: self.type <~< V[ND4J, ParametrisedDiff]
  //   ): ParametrisedDiff[P, PA, ((A, B), C), GP, GA] = ???

  //   def apply[
  //     P, A, B, C, PB, GP, GA, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  //   ](
  //     f: PDag.AssocLR[P, A, B, C, PB, GP, GA, V]
  //   )(
  //     implicit ev: self.type <~< V[ND4J, ParametrisedDiff]
  //   ): ParametrisedDiff[P, (A, (B, C)), PB, GP, GA] = ???

  //   def apply[
  //     P, A, B, C, PB, GP, GA, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]
  //   ](
  //     f: PDag.AssocLL[P, A, B, C, PB, GP, GA, V]
  //   )(
  //     implicit ev: self.type <~< V[ND4J, ParametrisedDiff]
  //   ): ParametrisedDiff[P, ((A, B), C), PB, GP, GA] = new ParametrisedDiff[P, ((A, B), C), PB, GP, GA] {
  //     val pf = f.dag.compile(ev.coerce(self))
  //     def apply(p: P, a: ((A, B), C)): PB = pf(p, (a._1._1, (a._1._2, a._2)))
  //     def gradP(p: P, a: ((A, B), C)): GP = pf.gradP(p, (a._1._1, (a._1._2, a._2)))
  //     def gradA(p: P, a: ((A, B), C)): GA = pf.gradA(p, (a._1._1, (a._1._2, a._2)))
  //   }

  //   def apply[
  //     S, D <: Dim2[_, _]
  //   , V[ctx, o[p, a, b, gp, ga]] <: NNet.Compiler[ctx, o]
  //   ](
  //     sigmoid: NNet.Sigmoid[S, D, V]
  //   )(implicit ev: self.type <~< V[ND4J, ParametrisedDiff]): ParametrisedDiff[HNil, Mat[S, D], Mat[S, D], HNil, Mat[S, D]] =
  //     new ParametrisedDiff[
  //       HNil, Mat[S, D], Mat[S, D]
  //     , HNil, Mat[S, D]
  //     ] {
  //       def apply(p: HNil, a: Mat[S, D]) = new Mat[S, D](
  //         Nd4j.getExecutioner().execAndReturn(new org.nd4j.linalg.api.ops.impl.transforms.Sigmoid(a.value))
  //       )

  //       def gradP(p: HNil, a: Mat[S, D]): HNil = HNil

  //       def gradA(p: HNil, a: Mat[S, D]) = new Mat[S, D](
  //         Nd4j.getExecutioner().execAndReturn(new org.nd4j.linalg.api.ops.impl.transforms.SigmoidDerivative(a.value))
  //       )
  //     }

  //   def apply[
  //     S <: Singleton, R, InR <: XInt, OutR <: XInt, OutC <: XInt
  //   , V[ctx, o[p, a, b, gp, ga]] <: NNet.Compiler[ctx, o]
  //   ](
  //     dense: NNet.DenseLayer[S, R, InR, OutR, OutC, V]
  //   )(implicit ev: self.type <~< V[ND4J, ParametrisedDiff]): ParametrisedDiff[
  //     FieldType[S, Mat[R, OutR x InR]] :: HNil, Mat[R, InR x OutC]
  //   , Mat[R, OutR x OutC]
  //   , FieldType[S, Mat[R, InR x OutC]] :: HNil
  //   , Mat[R, InR x OutR]
  //   ] = new ParametrisedDiff[
  //     FieldType[S, Mat[R, OutR x InR]] :: HNil
  //   , Mat[R, InR x OutC]
  //   , Mat[R, OutR x OutC]
  //   , FieldType[S, Mat[R, InR x OutC]] :: HNil
  //   , Mat[R, InR x OutR]
  //   ] {
  //     def apply(weights: FieldType[S, Mat[R, OutR x InR]] :: HNil, in: Mat[R, InR x OutC]): Mat[R, OutR x OutC] = {
  //       new Mat[R, OutR x OutC](weights.head.value.mmul(in.value))
  //     }

  //     def gradP(weights: FieldType[S, Mat[R, OutR x InR]] :: HNil, in: Mat[R, InR x OutC]): FieldType[S, Mat[R, InR x OutC]] :: HNil = {
  //       shapeless.labelled.field[S](in) :: HNil
  //     }

  //     def gradA(weights: FieldType[S, Mat[R, OutR x InR]] :: HNil, in: Mat[R, InR x OutC]): Mat[R, InR x OutR] = {
  //       weights.head.transpose
  //     }
  //   }
  // }

  // val net = nnetlayer[Double, 1, 1, 3]

  // val i = net.compile(ND4JTrans)


  // @inline def implement[P, PA, PB, PGP, PGA, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]](
  //   idag: PDag[P, PA, PB, PGP, PGA, V]
  // ): PDag[HNil, (P, PA), PB, HNil, HNil, V] = PDag.dagify(idag)

  // trait Learn[P, A, B, GP, GA, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o]] {
  //   def implement: PDag[HNil, (P, A), B, HNil, HNil, V]
  //   def update: PDag[HNil, ((P, A), B), P, HNil, HNil, V]
  //   def request: PDag[HNil, ((P, A), B), A, HNil, HNil, V]
  // }

  // def update[P : MultiplicativeMonoid : Comonoid, PA: MultiplicativeMonoid : Comonoid, PB : MultiplicativeMonoid, PGP, PGA, V[ctx, o[p, a, b, gp, ga]] <: PDag.Compiler[ctx, o], S](
  //   idag: PDag[P, PA, PB, PGP, PGA, V]
  // , costDiff: PDag[HNil, (PB, PB), PB, HNil, HNil, V]
  // , eps: S
  // )(implicit
  //   merger: Merger.Aux[P, HNil, P]
  // , mergerpp: Merger.Aux[P, P, P]
  // , mergerg: Merger.Aux[PGP, HNil, PGP]
  // , merger3: Merger.Aux[HNil, P, P]
  // , mergerpb: Merger.Aux[HNil, PB, PB]
  // , mergerpb2: Merger.Aux[PB, HNil, PB]
  // , merger4: Merger.Aux[HNil, PGP, PGP]
  // , multGBPP: CanMult.Aux[PGP, PB, P, V]
  // , multSPP: CanMult.Aux[S, P, P, V]
  // , sub: CanSub[P, V]
  // ): PDag[HNil, (P, PA, PB), P, HNil, HNil, V] = {
  //   implicit def ppaMM = new MultiplicativeMonoid[(P, PA)] {
  //     def times(x: (P, PA), y: (P, PA)): (P, PA) = (MultiplicativeMonoid[P].times(x._1, y._1), MultiplicativeMonoid[PA].times(x._2, y._2))
  //     def one: (P, PA) = (MultiplicativeMonoid[P].one, MultiplicativeMonoid[PA].one)
  //   }

  //   implicit def unitMM = new MultiplicativeMonoid[Unit] {
  //     def times(x: Unit, y: Unit): Unit = ()
  //     def one: Unit = ()
  //   }

  //   implicit def ppaCM = new Comonoid[(P, PA)] {
  //     def Comonoidalsplit(m: (P, PA)): ((P, PA), (P, PA)) = {
  //       val p = Comonoid[P].Comonoidalsplit(m._1)
  //       val pa = Comonoid[PA].Comonoidalsplit(m._2)
  //       ((p._1, pa._1), (p._2, pa._2))
  //     }
  //     def destroy(m: (P, PA)): Unit = ()
  //   }
  //   val impl = implement(idag)
  //   val err = PDag.dagify((impl || PDag.Id[PB, V]) >>> costDiff)

  //   // val r: Int = (PDag.Const(eps) || ((PDag.GradPDag(idag) || err) >>> multGBPP())) >>> multSPP()

  //   // val p = (PDag.Id[P, V] || r) >>> sub()
  //   // val i: Int = PDag.dagify(
  //   //   PDag.AssocRR(
  //   //     PDag.AssocRR(PDag.dagify(PDag.ComonoidalSplit[P, V]) || PDag.dagify(PDag.Id[PA, V])) > PDag.ComonoidalSplit[(P, A), V] || PDag.Id[PB, V]
  //   //   )
  //   // )
  //   // val input = PDag.dagify(PDag.Id[Unit, V]) || PDag.dagify(PDag.AssocRR(PDag.dagify(PDag.ComonoidalSplit[(P, PA), V]) || PDag.Id[PB, V]))
    
  //   // val r2: Int = (PDag.Id[P, V] || (input >>> r)) >>> sub()
  //   // val r3: Int = (PDag.Id[P, V] || input) >>> p
  //   ???
  // }  

}
