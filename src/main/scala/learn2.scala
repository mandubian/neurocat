package neurocat
package learn2

import cats.arrow.Category
import cats.Functor
import cats.Show

import shapeless.{HList, HNil, ::, Nat}
import shapeless.ops.hlist.{Prepend, Take, Drop, Length}
import shapeless.labelled._
import spire.implicits._
import spire.math._
import spire.algebra._

import singleton.ops._
import org.nd4j.linalg.factory.Nd4j
import nd4j._

import idag5._


// trait Learn {
//   type P
//   type A
//   type B

//   /** the implement function simply computes an estimated B based on current Params hypothesis
//     * and a given value training A
//     */
//   // def implement: ParametrisedDag[P, A, B]

//   /** the update function updates current Params hypothesis based on training values (A, B)
//     *
//     * In the case of neural networks, it can be seen as a learning step updating Params(weights) hypothesis
//     * by gradient descent with respect to the error computed on current training (A, B) & params hypothesis.
//     */
//   // def requestUpdate: ParametrisedDag[P, (A, B), (P, A)]

// }


// case class ParallelLearn[
//   F <: Learn
// , G <: Learn
// , PQ
// ](
//   val f: F
// , val g: G
// ) extends Learn {
//   type P = PQ
//   type A = (f.A, g.A)
//   type B = (f.B, g.B)
// }

// object ParallelLearn {
//   implicit def parallelizable[
//     P, A, B
//   , Q, C, D
//   , F <: Learn, G <: Learn
//   , PQ
//   ](implicit
//     f: Exploder0.Aux[F, P, A, B]
//   , g: Exploder0.Aux[G, Q, C, D]
//   , mergePQ0: Merger.Aux[P, Q, PQ]
//   ) : Parallelizable.Aux[F, G, ParallelLearn[F, G, PQ]] = new Parallelizable[F, G] {
//     type Out = ParallelLearn[F, G, PQ]

//     def apply(f: F, g: G): Out = ParallelLearn[F, G, PQ](f, g)
//   }

//   implicit def exploder[
//     P, A0, C0
//   , Q, B0, D0
//   , F <: Learn
//   , G <: Learn
//   , PQ
//   ](implicit
//     f: Exploder0.Aux[F, P, A0, C0]
//   , g: Exploder0.Aux[G, Q, B0, D0]
//   , MergerPQ0: Merger.Aux[P, Q, PQ]
//   ): Exploder0.Aux[ParallelLearn[F, G, PQ], PQ, (A0, B0), (C0, D0)] =
//     new Exploder0[ParallelLearn[F, G, PQ]] {
//       type P = PQ
//       type A = (A0, B0)
//       type B = (C0, D0)
//     }
// }



// case class ComposeLearn[
//   F <: Learn
// , G <: Learn
// , PQ
// ](
//   val g: G
// , val f: F
// ) extends Learn {
//   type P = PQ
//   type A = f.A
//   type B = g.B
// }

// object ComposeLearn {
//   implicit def parallelizable[
//     A0, B, C
//   , P, GP, GA1
//   , Q, GQ, GA2
//   , F <: Learn
//   , G <: Learn
//   ](implicit
//     f: Exploder0.Aux[F, P, A0, B]
//   , g: Exploder0.Aux[G, Q, B, C]
//   , MergerPQ0: Merger[P, Q]
//   ) : Composable.Aux[F, G, ComposeLearn[F, G, MergerPQ0.PQ]] = new Composable[F, G] {
//     type Out = ComposeLearn[F, G, MergerPQ0.PQ]

//     def apply(g: G, f: F): Out = ComposeLearn[F, G, MergerPQ0.PQ](g, f)
//   }

//   implicit def exploder[
//     P, A0, B0
//   , Q, C0
//   , F <: Learn
//   , G <: Learn
//   , PQ
//   ](implicit
//     f: Exploder0.Aux[F, P, A0, B0]
//   , g: Exploder0.Aux[G, Q, B0, C0]
//   , MergerPQ0: Merger.Aux[P, Q, PQ]
//   ): Exploder0.Aux[ComposeLearn[F, G, PQ], PQ, A0, C0] =
//     new Exploder0[ComposeLearn[F, G, PQ]] {
//       type P = PQ
//       type A = A0
//       type B = C0
//     }
// }

// object Learn {
//   implicit class Ops[F <: Learn](f: F) {

//     def ||[G <: Learn](g: G)(
//       implicit par: Parallelizable[F, G]
//     ): par.Out = par(f, g)

//     def >>>[G <: Learn](g: G)(
//       implicit comp: Composable[F, G]
//     ): comp.Out = comp(g, f)
//   }
// }


trait Implement[F] {
  type Out <: PDag

  def implement(f: F): Out
}


object Implement {
  type Aux[F, O] = Implement[F] { type Out = O }
  
  implicit def ext[F <: PDag] = new Implement[F] {    
    type Out = F

    def implement(f: F): F = f
  }

  // implicit def par[
  //   F <: DiffPDag
  // , G <: DiffPDag
  // , PQ
  // , GP](
  //   implicit f: Implement[F], g: Implement[G]
  // ) = new Implement[ParallelDiffPDag[F, G, PQ, GP]] {
  //   type Out = F
  //   def implement: PDag.Aux[P, A, B] = f >>
  // }

  implicitly[Implement[Sigmoid[Double, 2 x 3]]]
  implicitly[Implement[DenseLayer["s", Double, 1, 1, 3]]]

  // implicitly[Sigmoid[Double, 2 x 3] <:< ExtDiffPDag[HNil, Mat[Double, 2 x 3], Mat[Double, 2 x 3], HNil, Mat[Double, 2 x 3]]]

  // pdag[HNil, Mat[Double, 2 x 3], Mat[Double, 2 x 3], HNil, Mat[Double, 2 x 3], Sigmoid[Double, 2 x 3]]

  
}


trait Update[F, B, S] {
  type Out <: DiffPDag
  def update(
    eps: S
  , costDiff: Dag[(B, B), B]
  , f: F
  ): Out
}

trait CanMult[A, B, C] {
  def apply(): Dag[(A, B), C]
}

trait CanSub[A] {
  def apply(): Dag[(A, A), A]
}

trait Norm[A, S] {
  def norm: S
}


object Update {
  import DiffPDag._
  implicit def upd[
    F <: ExtDiffPDag, S, P, A, B, GP, GA
  , Par <: DiffPDag, Cmp <: DiffPDag
  , Par2 <: DiffPDag, Cmp2 <: DiffPDag
  , Par3 <: DiffPDag, Cmp3 <: DiffPDag
  , Par4 <: DiffPDag, Cmp4 <: DiffPDag
  ](
    implicit
      fex : Exploder.Aux[F, P, A, B, GP, GA]
    , fi: Implement.Aux[F, F]
    , par: Parallelizable.Aux[F, idag5.Id[B], Par]
    , comp: Composable.Aux[Par, Dag[(B, B), B], Cmp]
    , par2: Parallelizable.Aux[GradP[F], Cmp, Par2]
    , multGBPP: CanMult[GP, B, P]
    , comp2: Composable.Aux[Par2, Dag[(GP, B), P], Cmp2]
    , multSPP: CanMult[S, P, P]
    , par3: Parallelizable.Aux[Const[S], Cmp2, Par3]
    , comp3: Composable.Aux[Par3, Dag[(S, P), P], Cmp3]
    , par4: Parallelizable.Aux[idag5.Id[P], Cmp3, Par4]
    , comp4: Composable.Aux[Par4, Dag[(P, P), P], Cmp4]
    , sub: CanSub[P]
  ) = new Update[F, B, S] {
    type Out = Cmp4
    def update(
      eps: S
    , costDiff: Dag[(B, B), B]
    , f: F
    ): Cmp4 = {
      // (P, A) => B
      val ff = fi.implement(f)
      // ((P, (A, B)) => (B, B)) >>> ((B, B) => B) => ((P, (A, B)) =>  B)
      val err = (ff || Id[B]) >>> costDiff
      // ((P, (A, (A, B)) => B) >>> ((GradP, B) => P) => ((P, (A, (A, B)) => P)
      val r: Cmp3 = (Const(eps) || ((GradP(ff) || err) >>> multGBPP())) >>> multSPP()

      (Id[P] || r) >>> sub()
    }
  }
}


// trait Request[F] {
//   type P
//   type A
//   type B
// }

// object Request {
//   implicit def pdag[F <: ExtDiffPDag, P0, A0, B0](
//     implicit ex: Exploder0.Aux[F, P0, A0, B0]
//   ) = new Request[F] {
//     type P = P0
//     type A = A0
//     type B = B0
//   }
// }


object TestUpd {
  type F = DenseLayer2[Double, 1, 1, 3]
  //, Mat[Double, 1 x 3], Double]
  implicit val canMultGPB: CanMult[Mat[Double, 1 x 3] :: HNil, Mat[Double, 1 x 3], Mat[Double, 1 x 1] :: HNil] = ???
  implicit val canMultSP: CanMult[Double, Mat[Double, 1 x 1] :: HNil, Mat[Double, 1 x 1] :: HNil] = ???
  implicit val canSub: CanSub[Mat[Double, 1 x 1] :: HNil] = ???
  implicitly[Exploder[F]]
  implicitly[Implement.Aux[F, F]]
  // val par = implicitly[Parallelizable.Aux[
  //   DenseLayer2[Double, 1, 1, 3]
  // , idag5.Id[Mat[Double, 1 x 3]]
  // , ParallelDiffPDag[
  //     DenseLayer2[Double, 1, 1, 3]
  //   , idag5.Id[Mat[Double, 1 x 3]]
  //   , Mat[Double, 1 x 3] :: HNil
  //   , (Mat[Double, 1 x 1], Void)
  //   ]
  // ]]
  // def cmp[R] = implicitly[Composable[par.Out, Dag[(Mat[Double, 1 x 3], Mat[Double, 1 x 3]), Mat[Double, 1 x 3]]]]
  // implicitly[Update[F, Mat[Double, 1 x 3], Double]]
}

