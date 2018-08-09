package sandbox

import cats._
import cats.data.Validated
import cats.data.Validated._
import cats.implicits._

object DataCheck {

  sealed trait Predicate[E, A] {
    import Predicate._

    def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)

    def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(func) =>
          func(a)

        case And(left, right) =>
          (left(a), right(a)).mapN((_, _) => a)

        case Or(left, right) =>
          left(a) match {
            case Valid(_) => Valid(a)
            case Invalid(e1) =>
              right(a) match {
                case Valid(_)    => Valid(a)
                case Invalid(e2) => Invalid(e1 |+| e2)
              }
          }
      }
  }

  object Predicate {
    final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A])
        extends Predicate[E, A]

    final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A])
        extends Predicate[E, A]

    final case class Pure[E, A](func: A => Validated[E, A])
        extends Predicate[E, A]
  }

  sealed trait Check[E, A, B] {
    import Check._

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]

    def map[C](func: B => C): Check[E, A, C] =
      Map(this, func)

    def flatMap[C](func: B => Check[E, A, C]): Check[E, A, C] =
      FlatMap(this, func)
  }

  object Check {
    def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] =
      Pure(pred)

    final case class Map[E, A, B, C](check: Check[E, A, B], func: B => C) extends Check[E, A, C] {
      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
        check(a).map(func)
    }

    final case class Pure[E, A](pred: Predicate[E, A]) extends Check[E, A, A] {
      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
        pred(a)
    }

    final case class FlatMap[E, A, B, C](check: Check[E, A, B], func: B => Check[E, A, C]) extends Check[E, A, C] {
      def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] =
        check(in) match {
          case Validated.Valid(b) => func(b)(in)
          case Validated.Invalid(e) => e.invalid[C]
        }
    }
  }
}
