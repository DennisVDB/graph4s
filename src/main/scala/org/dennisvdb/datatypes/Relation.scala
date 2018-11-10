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

  implicit val relationGraphK: GraphK[Relation] = new GraphK[Relation] {
    override def connect[A](g1: Relation[A], g2: Relation[A]): Relation[A] = {
      val newRelation = for { a <- g1.domain; b <- g2.domain } yield (a, b)
      Relation(g1.domain ++ g2.domain, g1.relation ++ g2.relation ++ newRelation)
    }

    override def pure[A](a: A): Relation[A] = Relation(Set(a), Set.empty)

    override def empty[A]: Relation[A] = Relation(Set.empty, Set.empty)

    override def combineK[A](x: Relation[A], y: Relation[A]): Relation[A] =
      Relation(x.domain ++ y.domain, x.relation ++ y.relation)

    // format: off
    override def ap[A, B](ff: Relation[A => B])(fa: Relation[A]): Relation[B] =
      Relation(
        ff.domain.zip(fa.domain).map { case (f, a) => f(a) },
        ff.relation.zip(fa.relation).map { case ((f1, f2), (a1, a2)) => (f1(a1), f2(a2)) }
      )
    // format: on
  }

  implicit val relationFunctor: Functor[Relation] = new Functor[Relation] {
    override def map[A, B](fa: Relation[A])(f: A => B): Relation[B] = fa match {
      case Relation(domain, relation) =>
        Relation(domain.map(f), relation.map {
          case (a1, a2) => (f(a1), f(a2))
        })
    }
  }
}
