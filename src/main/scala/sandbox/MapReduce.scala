package sandbox

import cats._
import cats.implicits._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object MapReduce {

  def foldMap_[A, B: Monoid](seq: Vector[A])(f: A => B): B =
    seq.foldLeft(Monoid.empty[B])(_ |+| f(_))

  def parFoldMap_[A, B: Monoid](values: Vector[A])(f: A => B): Future[B] = {
    val processors = Runtime.getRuntime.availableProcessors()
    foldMap_(values.grouped(values.length / processors).toVector) { group =>
      Future(foldMap_(group)(f))
    }
  }

  def parFoldMap[A, B: Monoid](values: Vector[A])(f: A => B): Future[B] = {
    val processors = Runtime.getRuntime.availableProcessors()
    values
      .grouped(values.length / processors)
      .toVector
      .foldMap(g => Future(g.foldMap(f)))
  }

}
