package org.dennisvdb.typeclasses
import cats.{Eq, FunctorFilter}
import simulacrum.typeclass

@typeclass trait GraphFilterK[G[_]] extends GraphFoldableK[G] with FunctorFilter[G] {
  def induce[A](ga: G[A])(p: A => Boolean): G[A] = filter(ga)(p)

  def removeVertex[A: Eq](ga: G[A], a: A): G[A] = induce(ga)(Eq[A].neqv(_, a))
}
