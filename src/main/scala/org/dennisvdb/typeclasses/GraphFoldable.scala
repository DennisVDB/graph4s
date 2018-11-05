package org.dennisvdb.typeclasses

import cats._
import cats.implicits._
import simulacrum.typeclass

@typeclass trait GraphFoldable[G[_]] extends GraphK[G] with Foldable[G] { self =>
  override def isEmpty[A](ga: G[A]): Boolean = super.isEmpty(ga)

  def hasVertex[A: Eq](ga: G[A], a: A)(implicit G: Foldable[G]): Boolean = ga.contains_(a)

  def vertexSet[A](ga: G[A]): Set[A] = foldLeft(ga, Set.empty[A])(_ + _)

  def mesh[A, B](as: TraversableOnce[A], bs: TraversableOnce[B]): G[(A, B)] = ???

  def box[A, B](ga: G[A], gb: G[B]): G[(A, B)] = {
    val xs = toList(gb).map(b => map(ga)((_, b)))
    val ys = toList(ga).map(a => map(gb)((a, _)))

    (xs ++ ys).foldRight(empty[(A, B)])(combineK)
  }

  def torus[A, B](as: TraversableOnce[A], bs: TraversableOnce[B]): G[(A, B)] = box(circuit(as), circuit(bs))
}
