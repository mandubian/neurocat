package neurocat
package idag2

import shapeless.{HList, HNil, ::, DepFn2}
import cats.Monoid
import spire.implicits._
import spire.math._
import spire.algebra._

// import shapeless.ops.hlist._
import shapeless.ops.record._
import shapeless.labelled._

// trait Diff[A, B] extends (A => B) {
//   type GradA
//   def diff(in: A): GradA
// }

trait Parametrised[P, A, B] extends Function2[P, A, B]

trait ParametrisedDiff[P, A, B] extends Parametrised[P, A, B] {
  // gradient/jacobian per P 
  type GradP
  // gradient/jacobian per A
  type GradA
  def gradP(p: P, a: A): GradP
  def gradA(p: P, a: A): GradA
}

trait PartialDiff2 {
  def apply[A](a1: A, a2: A): A
  def diff[A](a1: A, a2: A): A
}

trait PartialDiffInvertible2 extends PartialDiff2 {
  def diffInvert[A](a1: A, a2: A): A
}

trait CanMult[A, B, C] {
  def apply(a: A, b: B): C
}

trait Norm[A, S] {
  def norm: S
}


sealed trait IDag[P <: HList, A, B] extends ParametrisedDiff[P, A, B] {
  self =>
  type GradP <: HList

  def gradPDag: NoDiffPDag[P, A, GradP] = NoDiffPDag((p:P, a:A) => self.gradP(p, a))

  type GradA

  def gradADag: NoDiffPDag[P, A, GradA] = NoDiffPDag((p:P, a:A) => self.gradA(p, a))
}


case class PDag[P, A, B](f: ParametrisedDiff[P, A, B]) extends IDag[P :: HNil, A, B] {
  def apply(p: P :: HNil, a: A): B = f(p.head, a)

  type GradP = f.GradP :: HNil
  def gradP(p: P :: HNil, a: A): f.GradP :: HNil = f.gradP(p.head, a) :: HNil
  override def gradPDag: NoDiffPDag[P :: HNil, A, GradP] = NoDiffPDag((p:P :: HNil, a:A) => f.gradP(p.head, a) :: HNil)

  type GradA = f.GradA
  def gradA(p: P :: HNil, a: A): f.GradA = f.gradA(p.head, a)
  override def gradADag: NoDiffPDag[P :: HNil, A, GradA] = NoDiffPDag((p:P :: HNil, a:A) => f.gradA(p.head, a))
}


case class NoDiffPDag[P <: HList, A, B](f: (P, A) => B) extends IDag[P, A, B] {

  def apply(p: P, a: A): B = f(p, a)

  type GradP = HNil
  def gradP(p: P, a: A): HNil = HNil

  type GradA = HNil
  def gradA(p: P, a: A): HNil = HNil
}

case class Diff[A, B](f: Differentiable[A, B]) extends IDag[HNil, A, B] {

  def apply(p: HNil, a: A): B = f(a)

  type GradP = HNil
  def gradP(p: HNil, a: A): HNil = HNil
  // override def gradPDag: BasicDag[P, A, GradP] = BasicDag((p:P, a:A) => HNil)

  type GradA = B
  def gradA(p: HNil, a: A): B = f.diff(a)
  // override def gradADag: BasicDag[P, A, GradA] = BasicDag((p:P, a:A) => HNil)

  def diff: NoDiffPDag[HNil, A, B] = NoDiffPDag((h: HNil, a:A) => f.diff(a))
}

case class DiffInvert[A](f: PartialDifferentiableInvertible2[A, 1]) extends IDag[HNil, (A, A), A] {

  def apply(p: HNil, a: (A, A)): A = f(a._1)(a._2)

  type GradP = HNil
  def gradP(p: HNil, a: (A, A)): HNil = HNil
  // override def gradPDag: BasicDag[P, A, GradP] = BasicDag((p:P, a:A) => HNil)

  type GradA = A
  def gradA(p: HNil, a: (A, A)): A = f.diff(a._1)(a._2)
  // override def gradADag: BasicDag[P, A, GradA] = BasicDag((p:P, a:A) => HNil)

  def diff: NoDiffPDag[HNil, (A, A), A] = NoDiffPDag((h: HNil, a:(A, A)) => f.diff(a._1)(a._2))
  def diffInvert: NoDiffPDag[HNil, (A, A), A] = NoDiffPDag((h: HNil, a:(A, A)) => f.diffInvert(a._1)(a._2))
}


final case class Id[A : Monoid]() extends IDag[HNil, A, A] {
  def apply(a: A): A = a
  def apply(p: HNil, a: A): A = a

  type GradP = HNil
  def gradP(p: HNil, a: A): HNil = HNil

  type GradA = A
  def gradA(p: HNil, a: A): A = Monoid[A].empty
}

final case class Parallel[P <: HList, Q <: HList, A, B, C, D, PQ <: HList](
  f: IDag[P, A, C], g: IDag[Q, B, D]
, selP: PQ => P
, selQ: PQ => Q
) extends IDag[PQ, (A, B), (C, D)] {

  def apply(pq: PQ, ab: (A, B)): (C, D) = {
    (f(selP(pq), ab._1), g(selQ(pq), ab._2))
  }

  type GradP = f.GradP :: g.GradP
  def gradP(pq: PQ, ab: (A, B)): f.GradP :: g.GradP = f.gradP(selP(pq), ab._1) :: g.gradP(selQ(pq), ab._2)

  type GradA = (f.GradA, g.GradA)
  def gradA(pq: PQ, ab: (A, B)): (f.GradA, g.GradA) = (f.gradA(selP(pq), ab._1), g.gradA(selQ(pq), ab._2))
}

final case class Compose[P <: HList, Q <: HList, A, B, C, PQ <: HList](
  g: IDag[Q, B, C], f: IDag[P, A, B]
, selP: PQ => P
, selQ: PQ => Q
) extends IDag[PQ, A, C] {

  def apply(pq: PQ, a: A): C = g(selQ(pq), f(selP(pq), a))

  type GradP = f.GradP :: g.GradP
  def gradP(pq: PQ, a: A): f.GradP :: g.GradP = {
    val d = f.gradP(selP(pq), a)
    f.gradP(selP(pq), a) :: g.gradP(selQ(pq), f(selP(pq), a))
  }

  type GradA = (f.GradA, g.GradA)
  def gradA(pq: PQ, a: A): (f.GradA, g.GradA) = {
    val d = f.gradA(selP(pq), a)
    (f.gradA(selP(pq), a), g.gradA(selQ(pq), f(selP(pq), a)))
  }
}

// // // monoid should be like multiplicative monoid
// // final case class Split[A : Comonoid : Monoid]() extends IDag[HNil, A, (A, A)] {
// //   def apply(pq: HNil, a: A): (A, A) = Comonoid[A].split(a)

// //   type GradP = HNil
// //   def gradP(pq: HNil, a: A): HNil = HNil

// //   type GradA = (A, A)
// //   def gradA(pq: HNil, a: A): (A, A) = (Monoid[A].empty, Monoid[A].empty)

// // }


// // // monoid should be like multiplicative monoid
// // final case class JoinDiff[A : Monoid]() extends IDag[HNil, (A, A), A] {
// //   def apply(pq: HNil, a: (A, A)): A = Monoid[A].combine(a._1, a._2)

// //   type GradP = HNil
// //   def gradP(pq: HNil, a: (A, A)): HNil = HNil

// //   type GradA = A
// //   def gradA(pq: HNil, a: (A, A)): A = Monoid[A].empty
// // }


object IDag extends LowerOps {

  type Graded[P <: HList, A, B, GP <: HList, GA] = IDag[P, A, B] {
    type GradP = GP
    type GradA = GA
  }

//   // def apply[P <: HList, A, B](f: ParametrisedDiff[P, A, B]) = new IDag[P, A, B] {
//   //   def apply(p: P, a: A): B = f(p, a)

//   //   type GradP = f.GradP
//   //   def gradP(p: P, a: A): f.GradP = f.gradP(p, a)

//   //   type GradA = f.GradA
//   //   def gradA(p: P, a: A): f.GradA = f.gradA(p, a)
//   // }

//   def init[A : Monoid](f: () => A) = new IDag[HNil, HNil, A] {
//     def apply(params: HNil, in: HNil): A = f()

//     type GradP = HNil
//     def gradP(p: HNil, a: HNil): HNil = HNil

//     type GradA = A :: HNil
//     def gradA(p: HNil, a: HNil): A :: HNil = Monoid[A].empty :: HNil
//   }

//   def discard[A]() = new IDag[HNil, A, HNil] {

//     def apply(params: HNil, in: A): HNil = HNil

//     type GradP = HNil
//     def gradP(p: HNil, a: A): HNil = HNil

//     type GradA = HNil
//     def gradA(p: HNil, a: A): HNil = HNil
//   }

//   // def split[A]: IDag[HNil, A, (A, A)] = ???

//   // def joinDiff[A]: IDag[HNil, (A, A), A] = ???

  // def joinDiff[A, B, C](f: (A, B) => C): IDag[HNil, (A, B), C] = ???

  def split[A]: IDag[HNil, A, (A, A)] = ???

  def first[A, B]: IDag[HNil, (A, B), A] = ???

  def second[A, B]: IDag[HNil, (A, B), B] = ???

  def parallel[P <: HList, Q <: HList, A, B, C, D, PQ <: HList](
    f: IDag[P, A, C]
  , g: IDag[Q, B, D]
  )(implicit
    merger: MergerE.Aux[P, Q, PQ]
  ): IDag[PQ, (A, B), (C, D)] = Parallel(
    f, g, merger.left _, merger.right _
  )

  def compose[P <: HList, Q <: HList, A, B, C, PQ <: HList](
    f: IDag[Q, B, C]
  , g: IDag[P, A, B]
  )(implicit
    merger: MergerE.Aux[P, Q, PQ]
  ): IDag[PQ, A, C] = Compose(
    f, g, merger.left _, merger.right _
  )

  def andThen[P <: HList, Q <: HList, A, B, C, PQ <: HList](
    g: IDag[P, A, B]
  , f: IDag[Q, B, C]
  )(implicit
    merger: MergerE.Aux[P, Q, PQ]
  ): IDag[PQ, A, C] = Compose(
    f, g, merger.left _, merger.right _
  )

//   // /*
//   //  *  A -----> C       A ---  ---> D
//   //  *              ==>       \/
//   //  *              ==>       /\
//   //  *  B -----> D       B ---  ---> C
//   //  */
//   def braid[P <: HList, Q <: HList,  A, B, C, D](par: Parallel[P, Q, A, B, C, D]) = new IDag[P :: Q, (A, B), (D, C)] {

//     def apply(pq: P :: Q, ab: (A, B)): (D, C) = {
//       par(pq, ab).swap
//     }

//     type GradP = par.g.GradP :: par.f.GradP
//     def gradP(pq: P :: Q, ab: (A, B)): par.g.GradP :: par.f.GradP = {
//       val gp = par.gradP(pq, ab)
//       gp.tail :: gp.head
//     }
    
//     type GradA = par.g.GradA :: par.f.GradA
//     def gradA(pq: P :: Q, ab: (A, B)): par.g.GradA :: par.f.GradA = {
//       val ga = par.gradA(pq, ab)
//       ga.tail :: ga.head
//     }
//   }

  def commute[A, B]: IDag[HNil, (A, B), (B, A)] = ???

  def associateR[A, B, C]: IDag[HNil, ((A, B), C), (A, (B, C))] = ???
  def associateL[A, B, C]: IDag[HNil, (A, (B, C)), ((A, B), C)] = ???

  def leftRightAssociate[P <: HList, A, B, C, D](f: IDag[P, ((A, B), C), D]): IDag[P, (A, (B, C)), D] = ???


  def rightAssociate[P <: HList, A, B, C, D](f: IDag[P, D, ((A, B), C)]): IDag[P, D, (A, (B, C))] = ???
// new IDag[P ,(A, (B, C)), D] {

//     def apply(p: P, abc: (A, (B, C))): D = {
//       val (a, (b, c)) = abc
//       f(p, ((a, b), c))
//     }
    
//     type GradP = f.GradP
//     def gradP(p: P, abc: (A, (B, C))): f.GradP = {
//       val (a, (b, c)) = abc
//       f.gradP(p, ((a, b), c))
//     }

//     type GradA = f.GradA    
//     def gradA(p: P, abc: (A, (B, C))): f.GradA = {
//       val (a, (b, c)) = abc
//       f.gradA(p, ((a, b), c))
//     }
//   }

  def leftAssociate[P <: HList, A, B, C, D](f: IDag[P, D, (A, (B, C))]): IDag[P, D, ((A, B), C)] = ???
//   def leftAssociate[P <: HList, A, B, C, D](f: IDag[P, (A, (B, C)), D]) = new IDag[P, ((A, B), C), D] {

//     def apply(p: P, abc: ((A, B), C)): D = {
//       val ((a, b), c) = abc
//       f(p, (a, (b, c)))
//     }

//     type GradP = f.GradP
//     def gradP(p: P, abc: ((A, B), C)): f.GradP = {
//       val ((a, b), c) = abc
//       f.gradP(p, (a, (b, c)))
//     }
    
//     type GradA = f.GradA
//     def gradA(p: P, abc: ((A, B), C)): f.GradA = {
//       val ((a, b), c) = abc
//       f.gradA(p, (a, (b, c)))
//     }
//   }

// //   def init[P : Monoid](p: => P) = Init(() => p)

// //   // case class NNetLayer[M[a, d <: Dim], R, W <: Dim2[_, _], In <: Dim2[_, _], Out <: Dim2[_, _]](
// //   //   val activation: Differentiable[M[R, Out], M[R, Out]]
// //   // , val body: ParametrisedDifferentiable[M[R, W], M[R, In], M[R, Out]]
// //   // )

// //   def nnetLayer[M[a, d <: Dim], S, W <: Dim2[_, _], In <: Dim2[_, _], Out <: Dim2[_, _]](
// //     initWeights: M[S, W]
// //   , activation: Diff[M[S, Out], M[S, Out]]
// //   , body: ParametrisedDiff[M[S, W], M[S, In], M[S, Out]]
// //   )(implicit m: Monoid[M[S, W]]): IDag[M[S, In], M[S, Out]] = {
// //     Compose(
// //       Edge(activation)
// //     , ParametrisedDag(init(initWeights), body)
// //     )
// //   }


  // def idag2Learn[P <: HList, A, B, S, GP <: HList, GA <: HList](
  //   idag: IDag.Graded[P, A, B, GP, GA]
  // )(
  //   eps: S
  // , cost: PartialDiffInvertible2
  // )(implicit
  //   pmod: Module[P, S]
  // , amod: Module[A, S]
  // , multgpb: CanMult[B, GP, P]
  // , multgpa: CanMult[B, GA, A]
  // , rMultGroup: MultiplicativeGroup[S]
  // , bNorm: Norm[B, S]
  // ): Learn.Aux[P, A, B] = {
  //   new Learn[A, B] {
  //     type Params   = P

  //     def implement(params: P)(a: A): B = {
  //       idag(params, a)
  //     }

  //     def update(params: P)(a: A, b: B): P = {
  //       //UI(p, a, b) ≔ p − ε∇pEI(p, a, b)
  //       val estim: B = idag(params, a)
  //       val err: B = cost.diff[B](estim, b)
  //       val gradP: GP = idag.gradP(params, a)
  //       val updatedP: P = multgpb(err, gradP)
  //       val paramsUpd: P = pmod.timesr(updatedP, rMultGroup.times(eps, bNorm.norm))
  //       pmod.minus(params, paramsUpd)
  //     }
      
  //     def request(params: P)(a: A, b: B): A = {
  //       // rI(p, a, b) ≔ fa((1/α(m))*∇aEI(p, a, b))
  //       val estim: B = idag(params, a)
  //       val err: B = cost.diff[B](estim, b)
  //       val gradA: GA = idag.gradA(params, a)
  //       val updA: A = multgpa(err, gradA)
  //       val aUpdate: A = amod.timesr(updA, rMultGroup.reciprocal(bNorm.norm))
  //       cost.diffInvert[A](a, aUpdate)
  //     }

  //   }
  // }

  implicit class Ops[P <: HList, A, B](idag: IDag[P, A, B]) {
    def ||[Q <: HList, C, D](other: IDag[Q, C, D])(implicit
      merger: MergerE[P, Q]
    ): IDag[merger.Out, (A, C), (B, D)] = Parallel(
      idag, other, merger.left _, merger.right _
    )

    def >>>[Q <: HList, C](other: IDag[Q, B, C])(implicit
      merger: MergerE[P, Q]
    ): IDag[merger.Out, A, C] = Compose(
      other, idag, merger.left _, merger.right _
    )

    // def commuteL[T, U](implicit prf: A =:= (T, U)): IDag[P, (U, T), B] = ???
    // def commuteR[T, U](implicit prf: B =:= (T, U)): IDag[P, A, (U, T)] = ???

    // def assocLR[T, U, V](implicit prf: A =:= ((T, U), V)): IDag[P, (T, (U, V)), B] = ???
    // def assocLL[T, U, V](implicit prf: A =:= (T, (U, V))): IDag[P, ((T, U), V), B] = ???

    // def assocRR[T, U, V](implicit prf: B =:= ((T, U), V)): IDag[P, A, (T, (U, V))] = ???
    // def assocRL[T, U, V](implicit prf: B =:= (T, (U, V))): IDag[P, A, ((T, U), V)] = ???
  }


  implicit class CommuteL[P <: HList, A, B, C](idag: IDag[P, (A, B), C]) {
    def commuteL: IDag[P, (B, A), C] = ???
  }

  implicit class CommuteR[P <: HList, A, B, C](idag: IDag[P, C, (A, B)]) {
    def commuteR: IDag[P, C, (B, A)] = ???
  }


  implicit class AssocRL[P <: HList, A, B, C, D](idag: IDag[P, D, ((A, B), C)]) {
    def assocR: IDag[P, D, (A, (B, C))] = ???
  }

  implicit class AssocRR[P <: HList, A, B, C, D](idag: IDag[P, D, (A, (B, C))]) {
    def assocL: IDag[P, D, ((A, B), C)] = ???
  }

}

trait LowerOps {

  implicit class AssocLR[P <: HList, A, B, C, D](idag: IDag[P, ((A, B), C), D]) {
    def assocR: IDag[P, (A, (B, C)), D] = ???
  }

  implicit class AssocLL[P <: HList, A, B, C, D](idag: IDag[P, (A, (B, C)), D]) {
    def assocL: IDag[P, ((A, B), C), D] = ???
  }
}

object Learn2 {

  import IDag._

  def cost[A]: DiffInvert[A] = ???

  def updateRequest[P <: HList, A: Monoid, B : Monoid, S](
    eps: S
  , implement: IDag[P, A, B]
  )(
    implicit
      multgpb: CanMult[B, implement.GradP, P]
    , multgpa: CanMult[B, implement.GradA, A]
    , pmod: Module[P, S]
    , amod: Module[A, S]
    , rMultGroup: MultiplicativeGroup[S]
    , bNorm: Norm[B, S]
  ): IDag[P, (A, B), (P, A)] = {
    
    val plug: IDag[HNil, (A, B), ((A, (A, B)), A)] =
      (((Id[A] || Id[B]) >>> (split[A] || Id[B])).assocR >>> (split[A] || (Id[A] || Id[B]))).assocR.commuteR

    val estimB: IDag[P, (A, B), (B, B)] = implement || Id[B]
    val err: IDag[P, (A, B), B] = (implement || Id[B]) >>> cost[B].diff

    val e: IDag[P,((A, (A, B)), A),((implement.GradP, B), (B, (implement.GradA, A)))] = 
      ((implement.gradPDag || (err >>> split[B])).assocL ||
        (split[A] >>> (implement.gradADag || Id[A]))).assocR

    val gradP: IDag[HNil, (implement.GradP, B), P] = NoDiffPDag((h:HNil, bgp: (implement.GradP, B)) => pmod.timesr(multgpb.apply(bgp._2, bgp._1), rMultGroup.times(eps, bNorm.norm)))
    val gradA: IDag[HNil, (B, implement.GradA), A] = NoDiffPDag((h:HNil, bga: (B, implement.GradA)) => amod.timesr(multgpa.apply(bga._1, bga._2), rMultGroup.reciprocal(bNorm.norm)))

    val updatedP: IDag[P, (implement.GradP, B), P] = gradP >>> NoDiffPDag(pmod.minus _)
    val updatedA: IDag[HNil, (B, (implement.GradA, A)), A] =
      ((Id[A] || gradA) >>> cost[A].diffInvert).commuteL.assocR
    
    plug >>> e >>> (updatedP || updatedA)
  }

}


// // final case class Init[A : Monoid](f: () => A) extends IDag[HNil, Unit, A] {
// //   def apply(params: HNil, in: Unit): A = f()

// //   type GradP = HNil
// //   def gradP(p: HNil, a: Unit): HNil = HNil

// //   type GradA = A
// //   def gradA(p: HNil, a: Unit): A = Monoid[A].empty
// // }

// // final case class Discard[A]() extends IDag[HNil, A, Unit] {

// //   def apply(params: HNil, in: A): Unit = ()

// //   type GradP = HNil
// //   def gradP(p: HNil, a: A): HNil = HNil

// //   type GradA = Unit
// //   def gradA(p: HNil, a: A): Unit = Unit
// // }