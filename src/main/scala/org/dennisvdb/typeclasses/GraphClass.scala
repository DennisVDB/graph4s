package org.dennisvdb.typeclasses

import simulacrum._

import cats.kernel.Eq
//import spire.math.Numeric

import scala.language.implicitConversions

@typeclass
trait GraphClass[G] {
  type Vertex
  def empty: G
  @op("test") def vertex(v: Vertex): G
  @op("-+-") def overlay(g1: G, g2: G): G
  @op("-*-") def connect(g1: G, g2: G): G

  type Aux[G0, V] = GraphClass[G0] { type Vertex = V }
}

object GraphClass {
  import GraphClass.ops._

  type Aux[G, V] = GraphClass[G] { type Vertex = V }

  def edge[G, V](v1: V, v2: V)(implicit ev: GraphClass.Aux[G, V]): G = {
    import ev._
    vertex(v1) -*- vertex(v2)
  }

  def edges[G, V](es: TraversableOnce[(V, V)])(implicit ev: GraphClass.Aux[G, V]): G = {
    import ev._
    es.map { case (v1, v2) => edge(v1, v2) }.foldRight(empty)(overlay)
  }

  def vertices[G, V](vs: TraversableOnce[V])(implicit ev: GraphClass.Aux[G, V]): G = {
    import ev._
    vs.map(vertex).foldRight(empty)(overlay)
  }

  def clique[G, V](vs: TraversableOnce[V])(implicit ev: GraphClass.Aux[G, V]): G = {
    import ev._
    vs.map(vertex).foldRight(empty)(connect)
  }

  def graph[G, V](vs: TraversableOnce[V], es: TraversableOnce[(V, V)])(
      implicit ev: GraphClass.Aux[G, V]): G =
    ev.overlay(vertices(vs), edges(es))

  def isSubgraphOf[G: GraphClass](g1: G, g2: G)(implicit ev: Eq[G]): Boolean =
    ev.eqv(g1 -+- g2, g2)
}
