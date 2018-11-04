package org.dennisvdb.datatypes

import cats._
import org.dennisvdb.typeclasses.GraphK

sealed abstract class Graph[A]

final case class Empty[A]() extends Graph[A]
final case class Vertex[A](a: A) extends Graph[A]
final case class Overlay[A](ga1: Graph[A], ga2: Graph[A]) extends Graph[A]
final case class Connect[A](ga1: Graph[A], ga2: Graph[A]) extends Graph[A]

object Graph {
  import GraphK.ops._

  def fold[G[_]: GraphK, A](g: Graph[A]): G[A] = g match {
    case Empty()           => GraphK[G].empty
    case Vertex(a)         => GraphK[G].vertex(a)
    case Overlay(ga1, ga2) => fold(ga1) -+- fold(ga2)
    case Connect(ga1, ga2) => fold(ga1) -*- fold(ga2)
  }

  implicit def eqGraph[G[_]: GraphK, A](implicit Eq: Eq[G[A]]): Eq[Graph[A]] =
    new Eq[Graph[A]] {
      override def eqv(x: Graph[A], y: Graph[A]): Boolean = Eq.eqv(fold(x), fold(y))
    }
}
