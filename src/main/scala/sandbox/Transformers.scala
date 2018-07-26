package sandbox

import cats.data.EitherT

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Transformers extends App {

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match {
      case Some(value) => EitherT[Future, String, Int](Future(Right(value)))
      case None => EitherT[Future, String, Int](Future(Left(autobot)))
    }


}
