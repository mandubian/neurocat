package neurocat

import singleton.ops._

// sealed trait Record

// trait FieldOf[V] {
//   import record._

//   type F = FieldType[this.type, V]

//   def ->>(v: V): FieldType[this.type, V] = field[this.type](v)
// }

package object record {
  // type FieldType[K, +V] = V with KeyTag[K, V]
  // trait KeyTag[K, +V]

  // def field[K] = new FieldBuilder[K]

  type ->>[S <: XString, V] = V with S

  // class FieldBuilder[K] {
  //   def apply[V](v : V): FieldType[K, V] = v.asInstanceOf[FieldType[K, V]]
  // }
}

// case object RNil extends Record

// trait Field[S <: XString, V](v: V)

// implicit def cs[S <: XString](s: S): 

// case class ::[S <: XString, V, R <: Record](h: (S, V), t: R)