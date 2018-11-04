package org.dennisvdb.datatypes
import org.dennisvdb.datatypes.Tree.Forest

case class Tree[A](label: A, subForest: Forest[A])

object Tree {
  type Forest[A] = List[Tree[A]]
}

