package org.dennisvdb.typeclasses

import cats._
import cats.data._
import cats.implicits._
import org.dennisvdb.datatypes.Tree
import org.dennisvdb.datatypes.Tree.Forest
import simulacrum._

@typeclass
trait GraphK[G[_]] extends Traverse[G] with Alternative[G] with FunctorFilter[G] {
  @op("-*-") def connect[A](g1: G[A], g2: G[A]): G[A]

  def vertex[A](a: A): G[A] = pure(a)
  @op("-+-") def overlay[A](g1: G[A], g2: G[A]): G[A] = combineK(g1, g2)
}

object GraphK {
  import GraphK.ops._

  def edge[G[_]: GraphK, A](a1: A, a2: A): G[A] = GraphK[G].vertex(a1) -+- GraphK[G].vertex(a2)

  def edges[G[_]: GraphK, V](es: TraversableOnce[(V, V)]): G[V] =
    overlays(es.map { case (v1, v2) => edge(v1, v2) })

  def vertices[G[_]: GraphK, V](vs: TraversableOnce[V]): G[V] = overlays(vs.map(GraphK[G].vertex))

  def overlays[G[_]: GraphK, A](gas: TraversableOnce[G[A]]): G[A] =
    gas.foldRight(GraphK[G].empty[A])(GraphK[G].overlay)

  def connects[G[_]: GraphK, A](gas: TraversableOnce[G[A]]): G[A] =
    gas.foldRight(GraphK[G].empty[A])(GraphK[G].connect)

  def clique[G[_]: GraphK, V](vs: TraversableOnce[V]): G[V] = connects(vs.map(GraphK[G].vertex))

  def biclique[G[_]: GraphK, V](vs1: TraversableOnce[V], vs2: TraversableOnce[V]): G[V] =
    (vs1.toList, vs2.toList) match {
      case (vs, Nil)  => vertices(vs)
      case (Nil, vs)  => vertices(vs)
      case (vs1, vs2) => vertices(vs1) -*- vertices(vs2)
    }

  def graph[G[_]: GraphK, V](vs: TraversableOnce[V], es: TraversableOnce[(V, V)]): G[V] =
    vertices(vs) -+- edges(es)

  def isSubgraphOf[G[_]: GraphK, V](g1: G[V], g2: G[V])(implicit Eq: Eq[G[V]]): Boolean =
    Eq.eqv(g1 -+- g2, g2)

  def isEmpty[G[_]: GraphK, A](ga: G[A]): Boolean = GraphK[G].isEmpty(ga)

  def hasVertex[G[_]: GraphK, A: Eq](ga: G[A], a: A): Boolean = ga.contains_(a)

//  def hasEdge[G[_]: GraphK, A: Eq](ga: G[A], u: A, v: A)(implicit Eq: Eq[G[A]]) : Boolean = {
//    val f = isSubgraphOf[G, A](edge(u, v), _)
//    val f2 = (a: G[List[A]]) => induce(_)(hasVertex[G, List[A]](a, List(e._1, e._2)))
//
//
//    val t = induce(_)(f)
//  }

  def vertexSet[G[_]: GraphK, A](ga: G[A]): Set[A] = GraphK[G].foldLeft(ga, Set.empty[A])(_ + _)

  def path[G[_]: GraphK, V](vs: TraversableOnce[V]): G[V] = vs.toList match {
    case Nil     => ev.empty
    case List(v) => ev.vertex(v)
    case vs      => edges(vs.zip(vs.tail))
  }

  def circuit[G[_]: GraphK, V](vs: TraversableOnce[V]): G[V] =
    vs.toList match {
      case Nil => ev.empty
      case vs  => edges(vs.zip(vs.tail))
    }

  def star[G[_]: GraphK, V](v: V, vs: TraversableOnce[V]): G[V] =
    GraphK[G].vertex(v) -*- vertices(vs)

  def startTranspose[G[_]: GraphK, V](v: V, vs: TraversableOnce[V]): G[V] =
    vertices(vs) -*- GraphK[G].vertex(v)

  def tree[G[_]: GraphK, V](tree: Tree[V]): G[V] = tree match {
    case Tree(rootLabel, subForest) =>
      star(rootLabel, subForest.map(_.label)) -+- forest(subForest)
  }

  def forest[G[_]: GraphK, V](forest: Forest[V]): G[V] = overlays(forest.map(tree))

  def mesh[G[_]: GraphK, A, B](as: TraversableOnce[A], bs: TraversableOnce[B]): G[(A, B)] = ???

  def box[G[_]: GraphK, A, B](ga: G[A], gb: G[B]): G[(A, B)] = {
    val xs = GraphK[G].toList(gb).map(b => GraphK[G].map(ga)((_, b)))
    val ys = GraphK[G].toList(ga).map(a => GraphK[G].map(gb)((a, _)))

    (xs ++ ys).foldRight(GraphK[G].empty[(A, B)])(GraphK[G].combineK)
  }

  def induce[G[_]: GraphK, A](ga: G[A])(p: A => Boolean): G[A] = GraphK[G].filter(ga)(p)

  implicit def graphFunctor[G[_]]: Functor[G] = ???
}
