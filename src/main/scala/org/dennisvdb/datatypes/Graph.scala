package org.dennisvdb.datatypes

import cats._
import org.dennisvdb.typeclasses.GraphClass

sealed abstract class Graph[A]

final case class Empty[A]() extends Graph[A]
final case class Vertex[A](a: A) extends Graph[A]
final case class Overlay[A](ga1: Graph[A], ga2: Graph[A]) extends Graph[A]
final case class Connect[A](ga1: Graph[A], ga2: Graph[A]) extends Graph[A]

object Graph {
  def fold[G, A](g: Graph[A])(implicit ev: GraphClass.Aux[G, A]): G = g match {
    case Empty()           => ev.empty
    case Vertex(a)         => ev.vertex(a)
    case Overlay(ga1, ga2) => ev.connect(fold(ga1), fold(ga2))
    case Connect(ga1, ga2) => ev.connect(fold(ga1), fold(ga2))
  }

  implicit def eqGraph[G: Eq, A](implicit ev: GraphClass.Aux[G, A]): Eq[Graph[A]] =
    new Eq[Graph[A]] {
      override def eqv(x: Graph[A], y: Graph[A]): Boolean = Eq[G].eqv(fold(x), fold(y))
    }
}
