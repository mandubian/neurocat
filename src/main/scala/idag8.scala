package neurocat
package idag8

import shapeless.{HList, HNil, ::, DepFn2}
import cats.Monoid

import algebra.ring.{MultiplicativeMonoid, AdditiveMonoid}

// import shapeless.ops.record._

// import shapeless.record._
import shapeless.labelled._
import shapeless._ ; import syntax.singleton._ ; import record._

import cats.evidence._

import singleton.ops._
import org.nd4j.linalg.factory.Nd4j
import nd4j._
import typeclasses._


trait PDagAlgebra[Ctx, Out[p, a, b, gp, ga]] {
  self =>
  
  import PDag._

  def id[
    A : DiagOned
  , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[Ctx, out]
  ](
    dag: Id[Ctx, A, Alg]
  )(
    implicit ev: self.type <~< Alg[Out]
  ): Out[HNil, A, A, HNil, A]

  def prod[
    P, PA, PB, PGP, PGA
  , Q, QB, QGP, QGA
  , PQ
  , PQGP
  , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[Ctx, out]
  ](
    dag: ProdPDag[
      Ctx
    , P, PA, PB, PGP, PGA
    , Q, QB, QGP, QGA
    , PQ
    , PQGP
    , Alg
    ]
  )(
    implicit ev: self.type <~< Alg[Out]
  ): Out[PQ, PA, (PB, QB), PQGP, (PGA, QGA)]

  def compose[
    P, A, B, PGP, PGA
  , Q, C, QGP, QGA
  , PQ
  , PQGP
  , PQGA
  , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[Ctx, out]
  ](
    dag: ComposePDag[
      Ctx
    , P, A, B, PGP, PGA
    , Q, C, QGP, QGA
    , PQ
    , PQGP
    , PQGA
    , Alg
    ]
  )(
    implicit ev: self.type <~< Alg[Out]
  ): Out[PQ, A, C, PQGP, PQGA]


  def fst[
    A : DiagOned, B : Zeroed
  , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[Ctx, out]
  ](
    dag: FstPDag[Ctx, A, B, Alg]
  )(
    implicit ev: self.type <~< Alg[Out]
  ): Out[HNil, (A, B), A, HNil, A]

  def snd[
    A : Zeroed, B : DiagOned
  , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[Ctx, out]
  ](
    dag: SndPDag[Ctx, A, B, Alg]
  )(
    implicit ev: self.type <~< Alg[Out]
  ): Out[HNil, (A, B), B, HNil, B]


  def dagify[
    P, A, B, PGP, PGA, PA
  , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[Ctx, out]
  ](
    dag: Dag[
      Ctx
    , P, A, B, PGP, PGA
    , PA
    , Alg
    ]
  )(
    implicit ev: self.type <~< Alg[Out]
  ): Out[HNil, PA, B, HNil, HNil]


  def split[
    A : DiagOned : Comonoid
  , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[Ctx, out]
  ](
    dag: ComonoidalSplit[
      Ctx
    , A
    , Alg
    ]
  ): Out[HNil, A, (A, A), HNil, (A, A)]

  def join[
    A : DiagOned : AdditiveMonoid
  , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[Ctx, out]
  ](
    dag: MonoidalJoin[
      Ctx
    , A
    , Alg
    ]
  ): Out[HNil, (A, A), A, HNil, A]


}

// We need ot fix the Ctx immediately because P, A, B, GP, GA types will already belong to it...
// Abstracting on those types gives horrible types for now... maybe in a future iteration!
trait PDag[Ctx, P, A, B, GradP, GradA, Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[Ctx, out]] {

  // the ugly GADT patch
  def compile[Out[p, a, b, gp, ga], Alg2[out[p, a, b, gp, ga]] <: Alg[out]](
    compiler: Alg2[Out]
  )(
    implicit ev: compiler.type <~< Alg[Out]
  ): Out[P, A, B, GradP, GradA]

}

object PDag {
  case class Id[Ctx, A : DiagOned, Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[Ctx, out]]()
  extends PDag[Ctx, HNil, A, A, HNil, A, Alg] {

    def compile[Out[p, a, b, gp, ga], Alg2[out[p, a, b, gp, ga]] <: Alg[out]](
      compiler: Alg2[Out]
    )(
      implicit ev: compiler.type <~< Alg[Out]
    ): Out[HNil, A, A, HNil, A] = compiler.id(this)

  }

  abstract class ProdPDag[
    Ctx
  , P, PA, PB, PGP, PGA
  , Q, QB, QGP, QGA
  , PQ
  , PQGP
  , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[Ctx, out]
  ] private[neurocat] (
    val f: PDag[Ctx, P, PA, PB, PGP, PGA, Alg]
  , val g: PDag[Ctx, Q, PA, QB, QGP, QGA, Alg]
  ) extends PDag[Ctx, PQ, PA, (PB, QB), PQGP, (PGA, QGA), Alg] {

    def merger: Merger.Aux[P, Q, PQ]
    def mergerg: Merger.Aux[PGP, QGP, PQGP]

    def compile[Out[p, a, b, gp, ga], Alg2[out[p, a, b, gp, ga]] <: Alg[out]]( 
      compiler: Alg2[Out]
    )(
      implicit ev: compiler.type <~< Alg[Out]
    ): Out[PQ, PA, (PB, QB), PQGP, (PGA, QGA)] = compiler.prod(this)

  }


  abstract class ComposePDag[
    Ctx
  , P, A, B, PGP, PGA
  , Q, C, QGP, QGA
  , PQ
  , PQGP
  , PQGA
  , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[Ctx, out]
  ] private[neurocat] (
    val g: PDag[Ctx, Q, B, C, QGP, QGA, Alg]
  , val f: PDag[Ctx, P, A, B, PGP, PGA, Alg]
  ) extends PDag[Ctx, PQ, A, C, PQGP, PQGA, Alg] {

    def merger: Merger.Aux[P, Q, PQ]
    def mergerg: Merger.Aux[PGP, QGP, PQGP]
    def gcomp: GradCompose.Aux[Ctx, PGA, QGA, PQGA, Alg]

    def compile[Out[p, a, b, gp, ga], Alg2[out[p, a, b, gp, ga]] <: Alg[out]](
      compiler: Alg2[Out]
    )(
      implicit ev: compiler.type <~< Alg[Out]
    ): Out[PQ, A, C, PQGP, PQGA] = compiler.compose(this)

  }


  case class FstPDag[
    Ctx
  , A : DiagOned, B : Zeroed
  , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[Ctx, out]
  ]() extends PDag[
    Ctx, HNil, (A, B), A, HNil, A, Alg
  ] {

    def compile[Out[p, a, b, gp, ga], Alg2[out[p, a, b, gp, ga]] <: Alg[out]](
      compiler: Alg2[Out]
    )(
      implicit ev: compiler.type <~< Alg[Out]
    ): Out[HNil, (A, B), A, HNil, A] = compiler.fst(this)

  }

  case class SndPDag[
    Ctx
  , A : Zeroed, B : DiagOned
  , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[Ctx, out]
  ]() extends PDag[
    Ctx, HNil, (A, B), B, HNil, B, Alg
  ] {

    def compile[Out[p, a, b, gp, ga], Alg2[out[p, a, b, gp, ga]] <: Alg[out]](
      compiler: Alg2[Out]
    )(
      implicit ev: compiler.type <~< Alg[Out]
    ): Out[HNil, (A, B), B, HNil, B] = compiler.snd(this)

  }

  abstract class Dag[
    Ctx
  , P, A, B, PGP, PGA
  , PA
  , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[Ctx, out]
  ] private[neurocat](val dag: PDag[Ctx, P, A, B, PGP, PGA, Alg]) extends PDag[Ctx, HNil, PA, B, HNil, HNil, Alg]  {

    def tuplizer: Tuplizer.Aux[P, A, PA]

    def compile[Out[p, a, b, gp, ga], Alg2[out[p, a, b, gp, ga]] <: Alg[out]](
      compiler: Alg2[Out]
    )(
      implicit ev: compiler.type <~< Alg[Out]
    ): Out[HNil, PA, B, HNil, HNil] = compiler.dagify(this)

  }


  case class ComonoidalSplit[
    Ctx
  , A : DiagOned : Comonoid
  , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[Ctx, out]
  ]() extends PDag[Ctx, HNil, A, (A, A), HNil, (A, A), Alg] {

    def compile[Out[p, a, b, gp, ga], Alg2[out[p, a, b, gp, ga]] <: Alg[out]](
      compiler: Alg2[Out]
    )(
      implicit ev: compiler.type <~< Alg[Out]
    ): Out[HNil, A, (A, A), HNil, (A, A)] = compiler.split(this)

  }

  case class MonoidalJoin[
    Ctx
  , A : DiagOned : AdditiveMonoid
  , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[Ctx, out]
  ]() extends PDag[Ctx, HNil, (A, A), A, HNil, A, Alg] {

    def compile[Out[p, a, b, gp, ga], Alg2[out[p, a, b, gp, ga]] <: Alg[out]](
      compiler: Alg2[Out]
    )(
      implicit ev: compiler.type <~< Alg[Out]
    ): Out[HNil, (A, A), A, HNil, A] = compiler.join(this)

  }  

}

trait PDagDsl[Ctx, Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[Ctx, out]] {
  import PDag._

  implicit def tupleZeroed[A : Zeroed, B : Zeroed] =
    new Zeroed[(A, B)] {
      def zero = (Zeroed[A].zero, Zeroed[B].zero)
    }

  implicit def tupleOned[A : DiagOned, B : DiagOned] =
    new DiagOned[(A, B)] {
      def diagOne = (DiagOned[A].diagOne, DiagOned[B].diagOne)
    }

  def id[A : DiagOned]: PDag[Ctx, HNil, A, A, HNil, A, Alg] = Id()

  def prod[
      P, PA, PB, PGP, PGA
    , Q, QB, QGP, QGA
    , PQ
    , PQGP
    , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[Ctx, out]
  ](
    f: PDag[Ctx, P, PA, PB, PGP, PGA, Alg]
  , g: PDag[Ctx, Q, PA, QB, QGP, QGA, Alg]
  )(
    implicit merger0: Merger[P, Q], mergerg0: Merger[PGP, QGP]
  ): PDag[Ctx, merger0.Out, PA, (PB, QB), mergerg0.Out, (PGA, QGA), Alg]
  = new ProdPDag[
      Ctx
    , P, PA, PB, PGP, PGA
    , Q, QB, QGP, QGA
    , merger0.Out, mergerg0.Out
    , Alg
    ](f, g) {
      def merger = merger0
      def mergerg = mergerg0
    }

  def compose[
    P, A, B, PGP, PGA
  , Q, C, QGP, QGA
  ](g: PDag[Ctx, Q, B, C, QGP, QGA, Alg], f: PDag[Ctx, P, A, B, PGP, PGA, Alg])(
    implicit merger0: Merger[P, Q], mergerg0: Merger[PGP, QGP], gcomp0: GradCompose[Ctx, PGA, QGA, Alg]
  ): PDag[Ctx, merger0.Out, A, C, mergerg0.Out, gcomp0.Out, Alg]
  = new ComposePDag[
      Ctx
    , P, A, B, PGP, PGA
    , Q, C, QGP, QGA
    , merger0.Out, mergerg0.Out, gcomp0.Out
    , Alg
    ](g, f) {
      def merger = merger0
      def mergerg = mergerg0
      def gcomp = gcomp0
    }


  def fst[A : DiagOned, B : Zeroed]: PDag[Ctx, HNil, (A, B), A, HNil, A, Alg] = FstPDag[Ctx, A, B, Alg]()

  def snd[A : Zeroed, B : DiagOned]: PDag[Ctx, HNil, (A, B), B, HNil, B, Alg] = SndPDag[Ctx, A, B, Alg]()

  def dagify[
    P, A, B, PGP, PGA
  , PA
  ](f: PDag[Ctx, P, A, B, PGP, PGA, Alg])(
    implicit tuplizer0: Tuplizer[P, A]
  ): PDag[Ctx, HNil, tuplizer0.Out, B, HNil, HNil, Alg] = new Dag[Ctx, P, A, B, PGP, PGA, tuplizer0.Out, Alg](f) {
    def tuplizer = tuplizer0
  }

  def par[
    P, PA: Zeroed : DiagOned, PB, PGP, PGA
  , Q, QA: Zeroed : DiagOned, QB, QGP, QGA
  ](f: PDag[Ctx, P, PA, PB, PGP, PGA, Alg], g: PDag[Ctx, Q, QA, QB, QGP, QGA, Alg])(
    implicit merger0: Merger[P, Q], mergerg0: Merger[PGP, QGP]
    , gcompp: GradCompose[Ctx, PA, PGA, Alg]
    , gcompq: GradCompose[Ctx, QA, QGA, Alg]
  ): PDag[Ctx, merger0.Out, (PA, QA), (PB, QB), mergerg0.Out, (gcompp.Out, gcompq.Out), Alg] =
    prod(
      compose(f, fst[PA, QA])
    , compose(g, snd[PA, QA])
    )

  def assocR[
    A : Zeroed : DiagOned, B : DiagOned : Zeroed, C : DiagOned : Zeroed
  ](implicit
    gca: GradCompose.Aux[Ctx, (A, B), A, A, Alg]
  , gcb: GradCompose.Aux[Ctx, (A, B), B, B, Alg]
  ): PDag[Ctx, HNil, ((A, B), C), (A, (B, C)), HNil, (A, (B, C)), Alg] = {
    val pa = compose(fst[A, B], fst[(A, B), C])
    val pb = compose(snd[A, B], fst[(A, B), C])
    val pc = snd[(A, B), C]
    prod(pa, prod(pb, pc))
  }


  def assocL[
    A : Zeroed : DiagOned, B : DiagOned : Zeroed, C : DiagOned : Zeroed
  ](implicit
    gca: GradCompose.Aux[Ctx, (B, C), B, B, Alg]
  , gcb: GradCompose.Aux[Ctx, (B, C), C, C, Alg]
  ): PDag[Ctx, HNil, (A, (B, C)), ((A, B), C), HNil, ((A, B), C), Alg] = {
    val pb = compose(fst[B, C], snd[A, (B, C)])
    val pc = compose(snd[B, C], snd[A, (B, C)])
    val pa = fst[A, (B, C)]
    prod(prod(pa, pb), pc)
  }

  def flip[
    A : DiagOned : Zeroed, B : DiagOned : Zeroed
  ]: PDag[Ctx, HNil, (A, B), (B, A), HNil, (B, A), Alg] =
    prod(snd[A, B], fst[A, B])


  implicit class Ops[
    P, PA, PB, PGP, PGA
  ](
    idag: PDag[Ctx, P, PA, PB, PGP, PGA, Alg]
  ) {

    def >>>[Q, QC, QGP, QGA](other: PDag[Ctx, Q, PB, QC, QGP, QGA, Alg])(
      implicit merger: Merger[P, Q], mergerg: Merger[PGP, QGP], gcompa: GradCompose[Ctx, PGA, QGA, Alg]
    ): PDag[Ctx, merger.Out, PA, QC, mergerg.Out, gcompa.Out, Alg] = compose(other, idag)


    def **[Q, QB, QGP, QGA](other: PDag[Ctx, Q, PA, QB, QGP, QGA, Alg])(
      implicit merger: Merger[P, Q], mergerg: Merger[PGP, QGP]
    ): PDag[Ctx, merger.Out, PA, (PB, QB), mergerg.Out, (PGA, QGA), Alg] = prod(idag, other)

    def ||[Q, QA, QB, QGP, QGA](other: PDag[Ctx, Q, QA, QB, QGP, QGA, Alg])(
      implicit
      merger: Merger[P, Q]
    , mergerg: Merger[PGP, QGP]
    , mmpa: DiagOned[PA]
    , ampa: Zeroed[PA]
    , mmqa: DiagOned[QA]
    , amqa: Zeroed[QA]
    , cm: GradCompose[Ctx, PA, PGA, Alg]
    , cm2: GradCompose[Ctx, QA, QGA, Alg]
    ): PDag[Ctx, merger.Out, (PA, QA), (PB, QB), mergerg.Out, (cm.Out, cm2.Out), Alg] = par(idag, other)
  }
}

trait GradCompose[
  Ctx, F, G
, Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[Ctx, out]
] {
  type Out
  def apply(): PDag[Ctx, HNil, (G, F), Out, HNil, HNil, Alg]
}

object GradCompose {
  type Aux[
    Ctx, F, G, FG
  , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[Ctx, out]
  ] = GradCompose[Ctx, F, G, Alg] { type Out = FG }
}


trait ND4JPDagAlgebra[Out[p, a, b, gp, ga]] extends PDagAlgebra[ND4J, Out] {
  self =>
  def sigmoid[
    S, R <: XInt
  , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[ND4J, out]
  ](
    implicit ev: self.type <~< Alg[Out]
  ): Out[HNil, Mat[S, R x 1], Mat[S, R x 1], HNil, Mat[S, R x R]]

  def denseLayer[
    S <: Singleton, R, InR <: XInt, OutR <: XInt, OutC <: XInt
  , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[ND4J, out]
  ](
    implicit ev: self.type <~< Alg[Out]
  ): Out[
      FieldType[S, Mat[R, OutR x InR]] :: HNil
    , Mat[R, InR x OutC]
    , Mat[R, OutR x OutC]
    , FieldType[S, Mat[R, InR x OutC]] :: HNil
    , Mat[R, OutR x InR]
    ]

  def matMult[
    S, A <: XInt, B <: XInt, C <: XInt
  , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[ND4J, out]
  ](
    implicit ev: self.type <~< Alg[Out]
  ): Out[
      HNil, (Mat[S, A x B], Mat[S, B x C]), Mat[S, A x C], HNil, HNil
    ]
}

trait ND4J
object ND4J extends ND4J {
  case class MatMult[
    S, A <: XInt, B <: XInt, C <: XInt
  , Alg[out[p, a, b, gp, ga]] <: ND4JPDagAlgebra[out]
  ]()
  extends PDag[ND4J, HNil, (Mat[S, A x B], Mat[S, B x C]), Mat[S, A x C], HNil, HNil, Alg] {
    def compile[Out[p, a, b, gp, ga], Alg2[out[p, a, b, gp, ga]] <: Alg[out]](
      compiler: Alg2[Out]
    )(
      implicit ev: compiler.type <~< Alg[Out]
    ): Out[HNil, (Mat[S, A x B], Mat[S, B x C]), Mat[S, A x C], HNil, HNil] = ??? //compiler(this)
  }


  case class Sigmoid[
    S, R <: XInt
  , Alg[out[p, a, b, gp, ga]] <: ND4JPDagAlgebra[out]]()
  extends PDag[ND4J, HNil, Mat[S, R x 1], Mat[S, R x 1], HNil, Mat[S, R x R], Alg] {

    def compile[Out[p, a, b, gp, ga], Alg2[out[p, a, b, gp, ga]] <: Alg[out]](
      compiler: Alg2[Out]
    )(
      implicit ev: compiler.type <~< Alg[Out]
    ): Out[HNil, Mat[S, R x 1], Mat[S, R x 1], HNil, Mat[S, R x R]] =
      compiler.sigmoid
  }


  case class DenseLayer[
    S <: Singleton, R, InR <: XInt, OutR <: XInt, OutC <: XInt
  , Alg[out[p, a, b, gp, ga]] <: ND4JPDagAlgebra[out]
  ]()
    extends PDag[
      ND4J
    , FieldType[S, Mat[R, OutR x InR]] :: HNil
    , Mat[R, InR x OutC]
    , Mat[R, OutR x OutC]
    , FieldType[S, Mat[R, InR x OutC]] :: HNil
    , Mat[R, OutR x InR]
    , Alg
    ] {
      def compile[Out[p, a, b, gp, ga], Alg2[out[p, a, b, gp, ga]] <: Alg[out]](
        compiler: Alg2[Out]
      )(
        implicit ev: compiler.type <~< Alg[Out]
      ): Out[
        FieldType[S, Mat[R, OutR x InR]] :: HNil
      , Mat[R, InR x OutC]
      , Mat[R, OutR x OutC]
      , FieldType[S, Mat[R, InR x OutC]] :: HNil
      , Mat[R, OutR x InR]
      ] = compiler.denseLayer
    }

}

trait ND4JDsl[Alg[out[p, a, b, gp, ga]] <: ND4JPDagAlgebra[out]] extends PDagDsl[ND4J, Alg] {
  import ND4J._

  def sigmoid[S, R <: XInt]: PDag[ND4J, HNil, Mat[S, R x 1], Mat[S, R x 1], HNil, Mat[S, R x R], Alg] = Sigmoid()

  def denseLayer[
    S <: Singleton, R, In <: Dim2[_, _], Out <: Dim2[_, _]
  ](
    implicit same: Dim2SameCol[In, Out]
  ): PDag[
      ND4J
    , FieldType[S, Mat[R, same.OutR x same.In]] :: HNil
    , Mat[R, same.In x same.OutC]
    , Mat[R, same.OutR x same.OutC]
    , FieldType[S, Mat[R, same.In x same.OutC]] :: HNil
    , Mat[R, same.OutR x same.In]
    , Alg
    ]
    = DenseLayer()

  implicit def matZeroed[
    S, A <: XInt, B <: XInt
  ](implicit
    opRows: SafeInt[A], opCols: SafeInt[B]
  ) = new Zeroed[Mat[S, A x B]] {
    def zero = new Mat[S, A x B](Nd4j.zeros(Array(opRows.value, opCols.value), Nd4j.order()))
  }

  implicit def matDiagOned[
    S, A <: XInt
  ](implicit
    A: SafeInt[A]
  ) = new DiagOned[Mat[S, A x A]] {
    def diagOne = new Mat[S, A x A](Nd4j.diag(Nd4j.ones(A.value, 1)))
  }

  implicit def vectorDiagOned[
    S, A <: XInt
  ](implicit
    A: SafeInt[A]
  ) = new DiagOned[Mat[S, A x 1]] {
    def diagOne = new Mat[S, A x 1](Nd4j.ones(A.value, 1))
  }

  implicit def matGradCompose[
    S, A <: XInt, B <: XInt, C <: XInt
  // , Alg[out[p, a, b, gp, ga]] <: ND4JPDagAlgebra[out]
  ] = new GradCompose[ND4J, Mat[S, B x C], Mat[S, A x B], Alg] {
    type Out = Mat[S, A x C]
    def apply(): PDag[ND4J, HNil, (Mat[S, A x B], Mat[S, B x C]), Mat[S, A x C], HNil, HNil, Alg] = 
      ND4J.MatMult[S, A, B, C, Alg]()
  }

  implicit def matSquareGradCompose[
    S, A <: XInt
  // , Alg[out[p, a, b, gp, ga]] <: ND4JPDagAlgebra[out]
  ](implicit
      A: SafeInt[A]
  ) = new GradCompose[ND4J, Mat[S, A x A], Mat[S, A x A], Alg] {
    type Out = Mat[S, A x A]
    def apply(): PDag[ND4J, HNil, (Mat[S, A x A], Mat[S, A x A]), Mat[S, A x A], HNil, HNil, Alg] = 
      dagify(fst[Mat[S, A x A], Mat[S, A x A]])
  }

  implicit def matVectorGradCompose[
    S, A <: XInt
  // , Alg[out[p, a, b, gp, ga]] <: ND4JPDagAlgebra[out]
  ](implicit
      A: SafeInt[A]
  ) = new GradCompose[ND4J, Mat[S, A x 1], Mat[S, A x 1], Alg] {
    type Out = Mat[S, A x 1]
    def apply(): PDag[ND4J, HNil, (Mat[S, A x 1], Mat[S, A x 1]), Mat[S, A x 1], HNil, HNil, Alg] = 
      dagify(fst[Mat[S, A x 1], Mat[S, A x 1]])
  }
    
  
}



trait Learn[
  Ctx
, P, A, B, GP, GA
, Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[Ctx, out]
] {
  def implement: PDag[Ctx, HNil, (P, A), B, HNil, HNil, Alg]
  def update: PDag[Ctx, HNil, ((P, A), B), P, HNil, HNil, Alg]
  def request: PDag[Ctx, HNil, ((P, A), B), A, HNil, HNil, Alg]
}

trait LearnDSL[Ctx, Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[Ctx, out]] extends PDagDsl[Ctx, Alg] {
  def fromPDag[
    P : DiagOned, A, B : DiagOned, GP, GA, S
  ](
    dag: PDag[Ctx, P, A, B, GP, GA, Alg]
  , costDiff: PDag[Ctx, HNil, (B, B), B, HNil, HNil, Alg]
  , eps: S
  ): Learn[Ctx, P, A, B, GP, GA, Alg] =
    new Learn[Ctx, P, A, B, GP, GA, Alg]{
      def implement: PDag[Ctx, HNil, (P, A), B, HNil, HNil, Alg] = {
        dagify(dag)
      }

      def update: PDag[Ctx, HNil, ((P, A), B), P, HNil, HNil, Alg] = {
        val impl = implement

        // val err = dagify((impl || id[B]) >>> costDiff)

        ???
      }
      def request: PDag[Ctx, HNil, ((P, A), B), A, HNil, HNil, Alg] = ???
    }
}

object Test {

  // I'd like not to introduce ND4J now but I can't do that without having crazy type signature for now...
  // I need to consider that again when I have more experience about it...
  trait Algebra[Out[p, a, b, gp, ga]]
    extends ND4JPDagAlgebra[Out]

  val dsl = new ND4JDsl[Algebra] with PDagDsl[ND4J, Algebra]
  import dsl._

  val net = compose(id[Mat[Double, 3 x 1]], id[Mat[Double, 3 x 1]])

  def nnetlayer[S, InR <: XInt, OutR <: XInt] = 
    (denseLayer["s", S, InR x 1, OutR x 1] >>> sigmoid)
  // ||
  //     (denseLayer["t", S, InR x 1, OutR x 1] >>> sigmoid)



  object PDiffAlgebra extends Algebra[ParametrisedDiff] {    
    self =>

    import PDag._

    def denseLayer[
      S <: Singleton, R, InR <: XInt, OutR <: XInt, OutC <: XInt
    , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[ND4J, out]
    ](
      implicit ev: self.type <~< Alg[ParametrisedDiff]
    ): ParametrisedDiff[
      FieldType[S, Mat[R, OutR x InR]] :: HNil
    , Mat[R,InR x OutC]
    , Mat[R, OutR x OutC]
    , FieldType[S, Mat[R, InR x OutC]] :: HNil
    , Mat[R, OutR x InR]
    ] = new ParametrisedDiff[
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
    
    def matMult[
      S, A <: XInt, B <: XInt, C <: XInt
    , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[ND4J, out]
    ](
      implicit ev: self.type <~< Alg[ParametrisedDiff]
    ): ParametrisedDiff[HNil, (Mat[S, A x B], Mat[S, B x C]), Mat[S, A x C], HNil, HNil] =
      new ParametrisedDiff[HNil, (Mat[S, A x B], Mat[S, B x C]), Mat[S, A x C], HNil, HNil] {
        def apply(p: HNil, in: (Mat[S, A x B], Mat[S, B x C])): Mat[S, A x C] =
          new Mat[S, A x C](in._1.value.mmul(in._2.value))

        def gradP(p: HNil, in: (Mat[S, A x B], Mat[S, B x C])): HNil = HNil

        def gradA(p: HNil, in: (Mat[S, A x B], Mat[S, B x C])): HNil = HNil
      }

    def sigmoid[
      S, R <: XInt
    , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[ND4J, out]
    ](
      implicit ev: self.type <~< Alg[ParametrisedDiff]
    ): ParametrisedDiff[HNil, Mat[S, R x 1], Mat[S, R x 1], HNil, Mat[S, R x R]] =
     new ParametrisedDiff[HNil, Mat[S, R x 1], Mat[S, R x 1], HNil, Mat[S, R x R]] {
        def apply(p: HNil, a: Mat[S, R x 1]) = new Mat[S, R x 1](
          Nd4j.getExecutioner().execAndReturn(new org.nd4j.linalg.api.ops.impl.transforms.Sigmoid(a.value))
        )

        def gradP(p: HNil, a: Mat[S, R x 1]): HNil = HNil

        def gradA(p: HNil, a: Mat[S, R x 1]): Mat[S, R x R] = {
          val v = Nd4j.getExecutioner().execAndReturn(new org.nd4j.linalg.api.ops.impl.transforms.SigmoidDerivative(a.value))
          new Mat[S, R x R](
            Nd4j.diag(v)
          )
        }
     }
    
    def prod[
      P, PA, PB, PGP, PGA, Q, QB, QGP, QGA, PQ, PQGP
    , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[ND4J, out]
    ](
      dag: ProdPDag[
        ND4J
      , P, PA, PB, PGP, PGA
      , Q, QB, QGP, QGA
      , PQ
      , PQGP
      , Alg
      ]
    )(
      implicit ev: self.type <~< Alg[ParametrisedDiff]
    ): ParametrisedDiff[PQ, PA, (PB, QB), PQGP, (PGA, QGA)] =
      new ParametrisedDiff[PQ, PA, (PB, QB), PQGP, (PGA, QGA)] {
        val pf = dag.f.compile(ev.coerce(self))
        val pg = dag.g.compile(ev.coerce(self))

        def apply(pq: PQ, ab: PA): (PB, QB) = {
          ( pf(dag.merger.left(pq), ab)
          , pg(dag.merger.right(pq), ab)
          )
        }

        def gradP(pq: PQ, ab: PA): PQGP = {
          dag.mergerg(pf.gradP(dag.merger.left(pq), ab), pg.gradP(dag.merger.right(pq), ab))
        }

        def gradA(pq: PQ, ab: PA): (PGA, QGA) = {
          (pf.gradA(dag.merger.left(pq), ab), pg.gradA(dag.merger.right(pq), ab))
        }
      }

    // Members declared in neurocat.idag8.PDagAlgebra
    def compose[
      P, A, B, PGP, PGA, Q, C, QGP, QGA, PQ, PQGP, PQGA
    , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[ND4J, out]
    ](
      dag: ComposePDag[
        ND4J
      , P, A, B, PGP, PGA
      , Q, C, QGP, QGA
      , PQ
      , PQGP
      , PQGA
      , Alg
      ]
    )(
      implicit ev: self.type <~< Alg[ParametrisedDiff]
    ): ParametrisedDiff[PQ, A, C, PQGP, PQGA] =
      new ParametrisedDiff[PQ, A, C, PQGP, PQGA] {
        val pf = dag.f.compile(ev.coerce(self))
        val pg = dag.g.compile(ev.coerce(self))
        val gcomp = dag.gcomp().compile(ev.coerce(self))

        def apply(pq: PQ, a: A): C = {
          pg(dag.merger.right(pq), pf(dag.merger.left(pq), a))
        }

        def gradP(pq: PQ, a: A): PQGP = {
          dag.mergerg(pf.gradP(dag.merger.left(pq), a), pg.gradP(dag.merger.right(pq), pf(dag.merger.left(pq), a)))
        }

        def gradA(pq: PQ, a: A): PQGA = {
          gcomp(
            HNil
          , pg.gradA(dag.merger.right(pq), pf(dag.merger.left(pq), a)) -> pf.gradA(dag.merger.left(pq), a)
          )
        }
      }

    def dagify[
      P, A, B, PGP, PGA, PA
    , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[ND4J, out]
    ](
      dag: Dag[ND4J, P, A, B, PGP, PGA, PA, Alg]
    )(
      implicit ev: self.type <~< Alg[ParametrisedDiff]
    ): ParametrisedDiff[HNil, PA, B, HNil, HNil] =
      new ParametrisedDiff[HNil, PA, B, HNil, HNil] {
        val pf = dag.dag.compile(ev.coerce(self))

        def apply(p: HNil, pa: PA): B = {
          val (p, a) = dag.tuplizer.untuplize(pa)
          pf(p, a)
        }

        def gradP(p: HNil, a: PA): HNil = HNil

        def gradA(p: HNil, a: PA): HNil = HNil
      }


    def fst[
      A : DiagOned, B : Zeroed
    , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[ND4J, out]
    ](
      dag: FstPDag[ND4J, A, B, Alg]
    )(
      implicit ev: self.type <~< Alg[ParametrisedDiff]
    ): ParametrisedDiff[HNil, (A, B), A, HNil, A] = ???

    def snd[
      A : Zeroed, B : DiagOned
    , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[ND4J, out]
    ](
      dag: SndPDag[ND4J, A, B, Alg]
    )(
      implicit ev: self.type <~< Alg[ParametrisedDiff]
    ): ParametrisedDiff[HNil, (A, B), B, HNil, B] = ???

    def id[
      A : DiagOned
    , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[ND4J, out]
    ](
      dag: Id[ND4J, A, Alg]
    )(
      implicit ev: self.type <~< Alg[ParametrisedDiff]
    ): ParametrisedDiff[HNil, A, A, HNil, A] =
      new ParametrisedDiff[HNil, A, A, HNil, A] {
        def apply(pq: HNil, a: A): A = a

        def gradP(p: HNil, a: A): HNil = HNil
        def gradA(p: HNil, a: A): A = DiagOned[A].diagOne
      }
    
    def split[
      A : DiagOned : Comonoid
    , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[ND4J, out]
    ](
      dag: ComonoidalSplit[
        ND4J
      , A
      , Alg
      ]
    ): ParametrisedDiff[HNil, A, (A, A), HNil, (A, A)] =
      new ParametrisedDiff[HNil, A, (A, A), HNil, (A, A)] {
        def apply(pq: HNil, a: A): (A, A) = Comonoid[A].split(a)

        def gradP(p: HNil, a: A): HNil = HNil
        def gradA(p: HNil, a: A): (A, A) = (DiagOned[A].diagOne, DiagOned[A].diagOne)
      }

    def join[
      A : DiagOned : AdditiveMonoid
    , Alg[out[p, a, b, gp, ga]] <: PDagAlgebra[ND4J, out]
    ](
      dag: MonoidalJoin[
        ND4J
      , A
      , Alg
      ]
    ): ParametrisedDiff[HNil, (A, A), A, HNil, A] =
      new ParametrisedDiff[HNil, (A, A), A, HNil, A] {
        def apply(pq: HNil, a: (A, A)): A = AdditiveMonoid[A].plus(a._1, a._2)

        def gradP(p: HNil, a: (A, A)): HNil = HNil
        def gradA(p: HNil, a: (A, A)): A = DiagOned[A].diagOne
      }
  }

   val n = nnetlayer[Double, 3, 4]

   val r = n.compile(PDiffAlgebra)

}