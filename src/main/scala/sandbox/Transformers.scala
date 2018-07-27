package sandbox

import cats.data.EitherT
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object Transformers extends App {

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def getPowerLevel(ally: String): Response[Int] =
    powerLevels.get(ally) match {
      case Some(value) => EitherT[Future, String, Int](Future(Right(value)))
      case None => EitherT[Future, String, Int](Future(Left(s"$ally out of range")))
    }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      p1 <- getPowerLevel(ally1)
      p2 <- getPowerLevel(ally2)
    } yield p1 + p2 > 10

  def tacticalReport(ally1: String, ally2: String): String =
    Await.result(
      canSpecialMove(ally1, ally2).value.collect {
        case Left(msg) => s"Comms error: $msg"
        case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
        case Right(false) => s"$ally1 and $ally2 need to recharge."
      },
      1.second
    )

  println(tacticalReport("Jazz", "Bumblebee"))
  println(tacticalReport("Hot Rod", "Bumblebee"))
  println(tacticalReport("Jazz", "Ironhide"))
}
