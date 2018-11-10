package org.dennisvdb.typeclasses

import simulacrum._
import cats.kernel.Eq
import org.dennisvdb.datatypes.Tree
import org.dennisvdb.datatypes.Tree.Forest

@typeclass trait GraphClass[G] {
  def empty: G
  def vertex[V](v: V)(implicit ev: V <:< G): G
  @op("-+-") def overlay(g1: G, g2: G): G
  @op("-*-") def connect(g1: G, g2: G): G

  def edge[V](v1: V, v2: V)(implicit ev: V <:< G): G = connect(vertex(v1), vertex(v2))

  def edges[V](es: TraversableOnce[(V, V)])(implicit ev: V <:< G): G =
    overlays(es.map { case (v1, v2) => edge(v1, v2) })

  def vertices[V](vs: TraversableOnce[V])(implicit ev: V <:< G): G = overlays(vs.map(v => vertex(v)))

  def overlays(gs: TraversableOnce[G]): G = gs.foldRight(empty)(overlay)

  def connects(gs: TraversableOnce[G]): G = gs.foldRight(empty)(connect)

  def isSubgraphOf(g1: G, g2: G)(implicit Eq: Eq[G]): Boolean = Eq.eqv(overlay(g1, g2), g2)

  def path[V](vs: TraversableOnce[V])(implicit ev: V <:< G): G = vs.toList match {
    case Nil     => empty
    case List(v) => vertex(v)
    case vs      => edges(vs.zip(vs.tail))
  }

  def circuit[V](vs: TraversableOnce[V])(implicit ev: V <:< G): G = vs.toList match {
    case Nil => empty
    case vs  => path(vs :+ vs.head)
  }

  def clique[V](vs: TraversableOnce[V])(implicit ev: V <:< G): G = connects(vs.map(v => vertex(v)))

  def biclique[V](vs1: TraversableOnce[V], vs2: TraversableOnce[V])(implicit ev: V <:< G): G =
    connect(vertices(vs1), vertices(vs2))

  def star[V](v: V, vs: TraversableOnce[V])(implicit ev: V <:< G): G = connect(vertex(v), vertices(vs))

  def startTranspose[V](v: V, vs: TraversableOnce[V])(implicit ev: V <:< G): G = connect(vertices(vs), vertex(v))

  def tree[V](t: Tree[V])(implicit ev: V <:< G): G = t match {
    case Tree(label, subForest) => overlay(star(label, subForest.map(_.label)), forest(subForest))
  }

  def forest[V](fs: Forest[V])(implicit ev: V <:< G): G = overlays(fs.map(f => tree(f)))
}

object GraphClass {
  implicit val instance: GraphClass[Int] = new GraphClass[Int] {
    override def empty: Int = 0
    override def vertex[V](v: V)(implicit ev: V <:< Int): Int = v
    override def overlay(g1: Int, g2: Int): Int = g1 + g2
    override def connect(g1: Int, g2: Int): Int = g1 * g2
  }
  GraphClass[Int].vertices(List(1, 2, 3))
}
