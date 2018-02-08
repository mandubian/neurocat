package neurocat 

import shapeless.{HList, HNil, ::, DepFn2}
import shapeless.ops.record._
import shapeless.labelled._


trait MergerE[L <: HList, M <: HList] extends DepFn2[L, M] with Serializable {
  type Out <: HList

  def left(out: Out): L
  def right(out: Out): M
}

trait LowPriorityMergerE3 {
  type Aux[L <: HList, M <: HList, Out0 <: HList] = MergerE[L, M] { type Out = Out0 }

  implicit def hnilRMergerE[M <: HList]: Aux[M, HNil, M] =
    new MergerE[M, HNil] {
      type Out = M
      def apply(l: M, m: HNil): Out = l
      def left(m: M): M = m
      def right(m: M): HNil = HNil
    }    

}

trait LowPriorityMergerE2 extends LowPriorityMergerE3 {

  implicit def hlistMergerE1[H, T <: HList, M <: HList]
    (implicit mt : MergerE[T, M]): Aux[H :: T, M, H :: mt.Out] =
      new MergerE[H :: T, M] {
        type Out = H :: mt.Out
        def apply(l: H :: T, m: M): Out = l.head :: mt(l.tail, m)

        def left(o: H :: mt.Out): H :: T = o.head :: mt.left(o.tail)
        def right(o: H :: mt.Out): M = mt.right(o.tail)
      }


}

trait LowPriorityMergerE extends LowPriorityMergerE2 {


  implicit def hnilLMergerE[M <: HList]: Aux[HNil, M, M] =
    new MergerE[HNil, M] {
      type Out = M
      def apply(l: HNil, m: M): Out = m
      def left(m: M): HNil = HNil
      def right(m: M): M = m
    }


  implicit def hlistMergerE2[K, V, T <: HList, M <: HList, MT <: HList]
    (implicit
      rm: Remover.Aux[M, K, (V, MT)]
    , rm2: Remove.Aux[M, FieldType[K, V], (FieldType[K, V], MT)]
    , mt: MergerE[T, MT]
    ): Aux[FieldType[K, V] :: T, M, FieldType[K, V] :: mt.Out] =
    new MergerE[FieldType[K, V] :: T, M] {
      type Out = FieldType[K, V] :: mt.Out
      def apply(l: FieldType[K, V] :: T, m: M): Out = {
        val (mv, mr) = rm(m)
        val up = field[K](mv)
        up :: mt(l.tail, mr)
      }

      def left(o: FieldType[K, V] :: mt.Out): FieldType[K, V] :: T = {
        o.head :: mt.left(o.tail)
      }

      def right(o: FieldType[K, V] :: mt.Out): M = {
        rm2.reinsert(o.head -> mt.right(o.tail))
      }
    }
}

object MergerE extends LowPriorityMergerE {
  def apply[L <: HList, M <: HList](implicit merger: MergerE[L, M]): Aux[L, M, merger.Out] = merger


  // No duplicates rule
  // In case you provide same list on both sides, just keep left side
  implicit def idMergerE[M <: HList]: Aux[M, M, M] =
    new MergerE[M, M] {
      type Out = M
      def apply(l: M, m: M): Out = l
      def left(m: M): M = m
      def right(m: M): M = m
    }

}