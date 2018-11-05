package org.dennisvdb.typeclasses
import cats.{Eq, FunctorFilter}
import simulacrum.typeclass

@typeclass trait GraphFilterK[G[_]] extends GraphFoldable[G] with FunctorFilter[G] {
  def induce[A](ga: G[A])(p: A => Boolean): G[A] = filter(ga)(p)

  def removeVertex[A: Eq](ga: G[A], a: A): G[A] = induce(ga)(Eq[A].neqv(_, a))

//    def hasEdge[A: Eq](ga: G[A], u: A, v: A) : Boolean = {
//      val f = isSubgraphOf[A](edge(u, v), _)
//      val f2 = (a: G[List[A]]) => induce(_)(hasVertex[List[A]](a, List(u, v)))
//
//      val t = induce(_)(f)
//    }
}

object A {
  import GraphFilterK.ops._

  def f[G[_]: GraphFilterK, A: Eq](ga: G[A], a: A): Unit =
    ga.removeVertex(a)
}
