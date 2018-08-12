package sandbox

import cats._
import cats.data.{Kleisli, NonEmptyList, Validated}
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

    def run(implicit s: Semigroup[E]): A => Either[E, A] =
      a => this(a).toEither
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

  type Result[A] = Either[Errors, A]

  type Check[A, B] = Kleisli[Result, A, B]

  def check[A, B](func: A => Result[B]): Check[A, B] =
    Kleisli(func)

  def checkPred[A](pred: Predicate[Errors, A]): Check[A, A] =
    Kleisli(pred.run)

  val checkUserName: Check[String, String] =
    checkPred(validateUserName)

  val checkEmail: Check[String, String] = {
    checkPred(containsOnce('@'))
      .map(_.split("@"))
      .andThen {
        check[Array[String], String] { arr =>
          (longerThan(0)(arr(0)), (longerThan(2) and contains('.'))(arr(1)))
            .mapN(_ |+| "@" |+| _)
            .toEither
        }
      }
  }
}
