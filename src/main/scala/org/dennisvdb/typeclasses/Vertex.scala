package org.dennisvdb.typeclasses
import simulacrum.typeclass

@typeclass
trait Vertex[G] {
  type V
  def vertex(v: V): G
}

object Vertex {
  type Aux[G, V0] = Vertex[G] { type V = V0 }
}
