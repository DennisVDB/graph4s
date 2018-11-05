//package org.dennisvdb.typeclasses
//
//import simulacrum._
//
//import cats.kernel.Eq
////import spire.math.Numeric
//
//@typeclass
//trait GraphClass[G, V] {
//  def empty: G
//  def vertex(v: V): G
//  @op("-+-") def overlay(g1: G, g2: G): G
//  @op("-*-") def connect(g1: G, g2: G): G
//}
//
//object GraphClass {
//  import GraphClass.ops._
//
//  def edge[G, V](v1: V, v2: V)(implicit ev: GraphClass.Aux[G, V]): G = {
//    import ev._
//    vertex(v1) -*- vertex(v2)
//  }
//
//  def edges[G, V](es: TraversableOnce[(V, V)])(implicit ev: GraphClass[G, V]): G = {
//    import ev._
//    es.map { case (v1, v2) => edge(v1, v2) }.foldRight(empty)(overlay)
//  }
//
//  def vertices[G, V](vs: TraversableOnce[V])(implicit ev: GraphClass[G, V]): G = {
//    import ev._
//    vs.map(vertex).foldRight(empty)(overlay)
//  }
//
//  def clique[G, V](vs: TraversableOnce[V])(implicit ev: GraphClass[G, V]): G = {
//    import ev._
//    vs.map(vertex).foldRight(empty)(connect)
//  }
//
//  def graph[G, V](vs: TraversableOnce[V], es: TraversableOnce[(V, V)])(implicit ev: GraphClass[G, V]): G =
//    ev.overlay(vertices(vs), edges(es))
//
//  def isSubgraphOf[G: GraphClass](g1: G, g2: G)(implicit ev: Eq[G]): Boolean =
//    ev.eqv(connect(g1, g2), g2)
//}