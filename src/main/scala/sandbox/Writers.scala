package sandbox

import sandbox.Writers.factorial

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationDouble
import scala.util.Random
import cats._
import cats.data.Writer
import cats.implicits._

object Writers {
  def slowly[A](body: => A): A =
    try body
    finally Thread.sleep(Random.nextInt(50) + 50)

  type Logged[T] = Writer[Vector[String], T]

  def factorial(n: Int): Logged[Int] = {
    for {
      ans <- if (n == 0) 1.pure[Logged]
      else slowly(factorial(n - 1).map(_ * n))
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  }
}

object WriterTest extends App {
  val r = Await.result(Future.sequence(
                         Vector(
                           Future(factorial(5)),
                           Future(factorial(5))
                         )),
                       5.seconds)

  println(r)
}
