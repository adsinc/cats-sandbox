package sandbox

import cats._
import cats.data.{NonEmptyList, Validated}
import cats.data.Validated._
import cats.implicits._
import sandbox.DataCheck.Predicate._

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

    def apply[E, A](f: A => Validated[E, A]): Predicate[E, A] =
      Pure(f)

    def lift[E, A](err: E, fn: A => Boolean): Predicate[E, A] =
      Pure(a => if (fn(a)) a.valid else err.invalid)

    type Errors = NonEmptyList[String]

    def error(s: String): Errors =
      NonEmptyList(s, Nil)

    def longerThan(n: Int): Predicate[Errors, String] =
      Predicate.lift(error(s"Must be longer than $n characters"), _.length > n)

    val alphanumeric: Predicate[Errors, String] =
      Predicate.lift(
        error(s"Must be all alphanumeric characters"),
        _.forall(_.isLetterOrDigit)
      )

    def contains(char: Char): Predicate[Errors, String] =
      Predicate.lift(error(s"Must contain character $char"), _.contains(char))

    def containsOnce(char: Char): Predicate[Errors, String] =
      Predicate.lift(
        error(s"Must contain character $char only once"),
        _.count(_ == char) == 1
      )

    val validateUserName: Predicate[Errors, String] =
      longerThan(3) and alphanumeric
  }

  sealed trait Check[E, A, B] {
    import Check._

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]

    def map[C](func: B => C): Check[E, A, C] =
      Map(this, func)

    def flatMap[C](func: B => Check[E, A, C]): Check[E, A, C] =
      FlatMap(this, func)

    def andThen[C](that: Check[E, B, C]): Check[E, A, C] =
      AndThen(this, that)
  }

  object Check {
    def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] =
      PurePredicate(pred)

    def apply[E, A, B](func: A => Validated[E, B]): Check[E, A, B] =
      Pure(func)

    final case class Map[E, A, B, C](check: Check[E, A, B], func: B => C)
        extends Check[E, A, C] {
      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
        check(a).map(func)
    }

    final case class FlatMap[E, A, B, C](check: Check[E, A, B],
                                         func: B => Check[E, A, C])
        extends Check[E, A, C] {
      def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] =
        check(in) match {
          case Validated.Valid(b)   => func(b)(in)
          case Validated.Invalid(e) => e.invalid[C]
        }
    }

    final case class AndThen[E, A, B, C](first: Check[E, A, B],
                                         second: Check[E, B, C])
        extends Check[E, A, C] {
      def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] =
        first(in).withEither(_.flatMap(b => second(b).toEither))
    }

    final case class Pure[E, A, B](func: A => Validated[E, B])
        extends Check[E, A, B] {
      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B] =
        func(a)
    }

    final case class PurePredicate[E, A](pred: Predicate[E, A])
        extends Check[E, A, A] {
      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
        pred(a)
    }

    val checkUserName: Check[Errors, String, String] =
      Check(validateUserName)

    val checkEmail: Check[Errors, String, String] = {
      Check(containsOnce('@'))
        .map(_.split("@"))
        .andThen {
          Check { arr =>
            (longerThan(0)(arr(0)), (longerThan(2) and contains('.'))(arr(1)))
              .mapN(_ |+| _)
          }
        }
    }
  }
}
