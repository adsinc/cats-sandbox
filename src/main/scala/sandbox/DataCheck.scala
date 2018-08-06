package sandbox

import cats._
import cats.implicits._

object DataCheck {

  sealed trait Check[E, A] {
    def and(that: Check[E, A]): Check[E, A] = And(this, that)

    def apply(value: A): Either[E, A]
  }

  final case class And[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]

  final case class CheckF[E: Semigroup, A](func: A => Either[E, A]) {
    def apply(value: A): Either[E, A] =
      func(value)

    def and(that: CheckF[E, A]): CheckF[E, A] =
      CheckF { a =>
        (this (a), that(a)) match {
          case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
          case (Left(e), Right(_)) => e.asLeft
          case (Right(_), Left(e)) => e.asLeft
          case (Right(_), Right(_)) => a.asRight
        }
      }
  }

}
