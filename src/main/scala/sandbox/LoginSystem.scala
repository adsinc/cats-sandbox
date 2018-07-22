package sandbox

import cats.data.Reader
import cats._
import cats.implicits._
import sandbox.LoginSystem.{Db, checkLogin}

object LoginSystem {

  case class Db(userNames: Map[Int, String],
                passwords: Map[String, String])

  type DbReader[A] = Reader[Db, A]

  def findUserName(userId: Int): DbReader[Option[String]] =
    Reader(_.userNames.get(userId))

  def checkPassword(userName: String, password: String): DbReader[Boolean] =
    Reader(_.passwords.get(userName).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = {
    findUserName(userId).flatMap {
      case Some(userName) => checkPassword(userName, password)
      case None => Reader(_ => false)
    }
  }

  def checkLoginFor(userId: Int, password: String): DbReader[Boolean] = {
    for {
      user <- findUserName(userId)
      result <- user
                  .map(userName => checkPassword(userName, password))
                  .getOrElse(false.pure[DbReader])
    } yield result
  }
}

object LoginSystemTest extends App {
  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)

  println(checkLogin(1, "zerocool").run(db))
  println(checkLogin(2, "ololo").run(db))
  println(checkLogin(4, "davinchi").run(db))
}

