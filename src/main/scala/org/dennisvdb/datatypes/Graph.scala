package org.dennisvdb

sealed abstract class Graph[A]

final case class Empty[A]() extends Graph[A]
final case class Vertex[A](a: A) extends Graph[A]
final case class Overlay[A](ga1: Graph[A], ga2: Graph[A]) extends Graph[A]
final case class Connect[A](ga1: Graph[A], ga2: Graph[A]) extends Graph[A]
