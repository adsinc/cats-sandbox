package sandbox

import cats._
import cats.data.Validated
import cats.implicits._

object DataCheck {

  sealed trait Predicate[E, A] {
    def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)

    def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(func) =>
          func(a)

        case And(left, right) =>
          (left(a), right(a)).mapN((_, _) => a)

        case Or(left, right) =>
          val l = left(a)
          val r = right(a)
          if (l.isValid || r.isValid) a.valid
          else (l, r).mapN((_, _) => a)
      }
  }

  final case class And[E, A](left: Predicate[E, A],
                             right: Predicate[E, A]) extends Predicate[E, A]

  final case class Or[E, A](left: Predicate[E, A],
                            right: Predicate[E, A]) extends Predicate[E, A]

  final case class Pure[E, A](func: A => Validated[E, A]) extends Predicate[E, A]

  sealed trait Check[E, A, B] {
    def apply(a: A): Validated[E, B] =
      ???

    def map[C](func: B => C): Check[E, A, C] =
      ???
  }

  val a: Predicate[List[String], Int] = Pure { v =>
    if (v > 2) v.valid
    else List("Must be > 2").invalid
  }

  val b: Predicate[List[String], Int] = Pure { v =>
    if (v < -2) v.valid
    else List("Must be < -2").invalid
  }

  val c: Predicate[List[String], Int] = Pure { v =>
    if (v == 0) v.valid
    else List("Must be = 0").invalid
  }

  val check: Predicate[List[String], Int] = a and b or c
}
