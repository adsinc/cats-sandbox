package sandbox

import cats._
import cats.data._
import cats.implicits._

object FormValidation {

  case class User(name: String, age: Int)

  type FormData = Map[String, String]
  type FailFast[A] = Either[List[String], A]
  type FailSlow[A] = Validated[List[String], A]

  def getValue(data: FormData)(valueName: String): FailFast[String] =
    data.get(valueName)
      .toRight(List(s"No value $valueName"))

  def parseInt(name: String)(value: String): FailFast[Int] =
    Either.catchOnly[NumberFormatException](value.toInt)
      .leftMap(_ => List(s"$name must be integer"))

  def nonBlank(name: String)(value: String): FailFast[String] =
    Either.right(value)
      .ensure(List(s"$name is empty"))(_.nonEmpty)

  def nonNegative(name: String)(value: Int): FailFast[Int] =
    Either.right(value)
      .ensure(List(s"$name can't be negative"))(_ >= 0)

  def readName(data: FormData): FailFast[String] =
    getValue(data)("name")
      .flatMap(nonBlank("name"))

  def readAge(data: FormData): FailFast[Int] =
    getValue(data)("age")
      .flatMap(nonBlank("age"))
      .flatMap(parseInt("age"))
      .flatMap(nonNegative("age"))

  def createUser(data: FormData): FailSlow[User] =
    Semigroupal.map2(readName(data).toValidated, readAge(data).toValidated)(User.apply)

}
