package sandbox

import cats._
import cats.data.Validated
import cats.implicits._

object DataCheck {

  sealed trait Check[E, A] {
    def and(that: Check[E, A]): Check[E, A] = And(this, that)

    def or(that: Check[E, A]): Check[E, A] = Or(this, that)

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

  final case class And[E, A](left: Check[E, A],
                             right: Check[E, A]) extends Check[E, A]

  final case class Or[E, A](left: Check[E, A],
                            right: Check[E, A]) extends Check[E, A]

  final case class Pure[E, A](func: A => Validated[E, A]) extends Check[E, A]

  val a: Check[List[String], Int] = Pure { v =>
    if (v > 2) v.valid
    else List("Must be > 2").invalid
  }

  val b: Check[List[String], Int] = Pure { v =>
    if (v < -2) v.valid
    else List("Must be < -2").invalid
  }

  val c: Check[List[String], Int] = Pure { v =>
    if (v == 0) v.valid
    else List("Must be = 0").invalid
  }

  val check: Check[List[String], Int] = a and b or c
}
