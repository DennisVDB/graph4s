package org.dennisvdb.typeclasses

import simulacrum._

import cats.kernel.Eq
//import spire.math.Numeric

import scala.language.implicitConversions

@typeclass
trait Graph[G] {
  type Vertex
  def empty: G
  @op("test") def vertex(v: Vertex): G
  @op("-+-") def overlay(g1: G, g2: G): G
  @op("-*-") def connect(g1: G, g2: G): G

  type Aux[G0, V] = Graph[G0] { type Vertex = V }
}

object Graph {
  import Graph.ops._

  type Aux[G, V] = Graph[G] { type Vertex = V }

  def edge[G, V](v1: V, v2: V)(implicit ev: Graph.Aux[G, V]): G = {
    import ev._
    vertex(v1) -*- vertex(v2)
  }

  def edges[G, V](es: TraversableOnce[(V, V)])(implicit ev: Graph.Aux[G, V]): G = {
    import ev._
    es.map { case (v1, v2) => edge(v1, v2) }.foldRight(empty)(overlay)
  }

  def vertices[G, V](vs: TraversableOnce[V])(implicit ev: Graph.Aux[G, V]): G = {
    import ev._
    vs.map(vertex).foldRight(empty)(overlay)
  }

  def clique[G, V](vs: TraversableOnce[V])(implicit ev: Graph.Aux[G, V]): G = {
    import ev._
    vs.map(vertex).foldRight(empty)(connect)
  }

  def graph[G, V](vs: TraversableOnce[V], es: TraversableOnce[(V, V)])(
      implicit ev: Graph.Aux[G, V]): G =
    ev.overlay(vertices(vs), edges(es))

  def isSubgraphOf[G: Graph](g1: G, g2: G)(implicit ev: Eq[G]): Boolean =
    ev.eqv(g1 -+- g2, g2)

//  implicit def numericG[G: Graph]: Numeric[G] = new Numeric[G] {
//    override def plus(x: G, y: G): G = x -+- y
//    override def minus(x: G, y: G): G =
//    override def times(x: G, y: G): G = x -*- y
//    override def negate(x: G): G = x
//    override def fromInt(x: Int): G = Graph.Aux[G, Int].vertex(x)
//    override def toInt(x: G): Int = ???
//    override def toLong(x: G): Long = ???
//    override def toFloat(x: G): Float = ???
//    override def toDouble(x: G): Double = ???
//    override def compare(x: G, y: G): Int = ???
//  }
}
