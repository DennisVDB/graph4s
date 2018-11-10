package org.dennisvdb.typeclasses

import cats._
import cats.implicits._
import simulacrum.typeclass

@typeclass trait GraphFoldableK[G[_]] extends GraphK[G] with Traverse[G] { self =>
  override def isEmpty[A](ga: G[A]): Boolean = super.isEmpty(ga)

  def hasVertex[A: Eq](ga: G[A], a: A)(implicit G: Foldable[G]): Boolean = ga.contains_(a)

  def replaceVertex[A: Eq](replaceA: A, withA: A, ga: G[A]): G[A] =
    map(ga) { a =>
      if (a == replaceA) withA else a
    }

  def mergeVertices[A](ga: G[A], mergedA: A)(p: A => Boolean): G[A] =
    map(ga) { a =>
      if (p(a)) mergedA else a
    }

  def vertexSet[A](ga: G[A]): Set[A] = foldLeft(ga, Set.empty[A])(_ + _)

  def box[A, B](ga: G[A], gb: G[B]): G[(A, B)] = {
    val xs = toList(gb).map(b => map(ga)((_, b)))
    val ys = toList(ga).map(a => map(gb)((a, _)))

    (xs ++ ys).foldRight(empty[(A, B)])(combineK)
  }

  def torus[A, B](as: TraversableOnce[A], bs: TraversableOnce[B]): G[(A, B)] = box(circuit(as), circuit(bs))

  def mesh[A, B](as: TraversableOnce[A], bs: TraversableOnce[B]): G[(A, B)] = box(path(as), path(bs))
}
