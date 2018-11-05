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

//  implicit val relationGraph: GraphK[Relation] = new GraphK[Relation] {
//    override def connect[A](r1: Relation[A], r2: Relation[A]): Relation[A] = {
//      val newRelations = for {
//        a <- r1.domain
//        b <- r2.domain
//      } yield (a, b)
//
//      Relation(r1.domain ++ r2.domain, r1.relation ++ r2.relation ++ newRelations)
//    }
//    override def foldLeft[A, B](fa: Relation[A], b: B)(f: (B, A) => B): B = fa match {
//      case Relation(domain, _) => domain.foldLeft(b)(f)
//    }
//
//    override def foldRight[A, B](fa: Relation[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa {
//      case Relation(domain, _) => domain.foldRight(lb)(f)
//    }
//
//    override def pure[A](x: A): Relation[A] = Relation(Set(x), Set.empty)
//
//    override def traverse[G[_], A, B](fa: Relation[A])(f: A => G[B])(
//      implicit evidence$1: Applicative[G]
//    ): G[Relation[B]] = ???
//
//    override def empty[A]: Relation[A] = ???
//
//    override def ap[A, B](ff: Relation[A => B])(fa: Relation[A]): Relation[B] = ???
//
//    override def mapFilter[A, B](fa: Relation[A])(f: A => Option[B]): Relation[B] = ???
//
//    override def combineK[A](x: Relation[A], y: Relation[A]): Relation[A] = ???
//  }

//  implicit def relationMonoid[A]: Monoid[Relation[A]] = new Monoid[Relation[A]] {
//    override def empty: Relation[A] = Relation(Set.empty, Set.empty)
//    override def combine(r1: Relation[A], r2: Relation[A]): Relation[A] =
//      Relation(r1.domain ++ r2.domain, r1.relation ++ r2.relation)
//  }

  implicit val relationFunctor: Functor[Relation] = new Functor[Relation] {
    override def map[A, B](fa: Relation[A])(f: A => B): Relation[B] = fa match {
      case Relation(domain, relation) =>
        Relation(domain.map(f), relation.map {
          case (a1, a2) => (f(a1), f(a2))
        })
    }
  }
}
