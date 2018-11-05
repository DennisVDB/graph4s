package org.dennisvdb.typeclasses

import cats._
import org.dennisvdb.datatypes.Tree
import org.dennisvdb.datatypes.Tree.Forest
import simulacrum._

@typeclass trait GraphK[G[_]] extends Alternative[G] { self =>
  @op("-*-") def connect[A](g1: G[A], g2: G[A]): G[A]

  def vertex[A](a: A): G[A] = pure(a)

  @op("-+-") def overlay[A](g1: G[A], g2: G[A]): G[A] = combineK(g1, g2)

  def edge[A](a1: A, a2: A): G[A] = connect(vertex(a1), vertex(a2))

  def edges[V](es: TraversableOnce[(V, V)]): G[V] = overlays(es.map { case (v1, v2) => edge(v1, v2) })

  def vertices[V](vs: TraversableOnce[V]): G[V] = overlays(vs.map(vertex))

  def overlays[A](gas: TraversableOnce[G[A]]): G[A] = gas.foldRight(empty[A])(overlay)

  def connects[A](gas: TraversableOnce[G[A]]): G[A] = gas.foldRight(empty[A])(connect)

  def clique[V](vs: TraversableOnce[V]): G[V] = connects(vs.map(vertex))

  def biclique[V](vs1: TraversableOnce[V], vs2: TraversableOnce[V]): G[V] =
    (vs1.toList, vs2.toList) match {
      case (vs, Nil)  => vertices(vs)
      case (Nil, vs)  => vertices(vs)
      case (vs1, vs2) => connect(vertices(vs1), vertices(vs2))
    }

  def path[V](vs: TraversableOnce[V]): G[V] = vs.toList match {
    case Nil     => empty
    case List(v) => vertex(v)
    case vs      => edges(vs.zip(vs.tail))
  }

  def circuit[V](vs: TraversableOnce[V]): G[V] = vs.toList match {
    case Nil => empty
    case vs  => edges(vs.zip(vs.tail))
  }

  def star[V](v: V, vs: TraversableOnce[V]): G[V] = connect(vertex(v), vertices(vs))

  def startTranspose[V](v: V, vs: TraversableOnce[V]): G[V] = connect(vertices(vs), vertex(v))

  def tree[V](tree: Tree[V]): G[V] = tree match {
    case Tree(rootLabel, subForest) =>
      connect(star(rootLabel, subForest.map(_.label)), forest(subForest))
  }

  def forest[V](forest: Forest[V]): G[V] = overlays(forest.map(tree))

  def graph[V](vs: TraversableOnce[V], es: TraversableOnce[(V, V)]): G[V] = overlay(vertices(vs), edges(es))

  def isSubgraphOf[V](g1: G[V], g2: G[V])(implicit Eq: Eq[G[V]]): Boolean = Eq.eqv(overlay(g1, g2), g2)
}
