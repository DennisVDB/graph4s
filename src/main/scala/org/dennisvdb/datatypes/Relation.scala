package org.dennisvdb.datatypes
import cats._
import cats.implicits._
import org.dennisvdb.typeclasses.GraphK

final case class Relation[A](domain: Set[A], relation: Set[(A, A)])

object Relation {
  implicit def relationEq[A: Eq]: Eq[Relation[A]] = new Eq[Relation[A]] {
    override def eqv(x: Relation[A], y: Relation[A]): Boolean =
      x.domain === y.domain && x.relation === y.relation
  }

  implicit val relationGraph: GraphK[Relation] = new GraphK[Relation] {
    override def connect[A](r1: Relation[A], r2: Relation[A]): Relation[A] = {
      val newRelations = for {
        a <- r1.domain
        b <- r2.domain
      } yield (a, b)

      Relation(r1.domain ++ r2.domain, r1.relation ++ r2.relation ++ newRelations)
    }
  }

//  implicit def relationMonoid[A]: Monoid[Relation[A]] = new Monoid[Relation[A]] {
//    override def empty: Relation[A] = Relation(Set.empty, Set.empty)
//    override def combine(r1: Relation[A], r2: Relation[A]): Relation[A] =
//      Relation(r1.domain ++ r2.domain, r1.relation ++ r2.relation)
//  }
}
