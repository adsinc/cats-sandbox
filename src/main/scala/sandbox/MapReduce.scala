package sandbox

import cats._
import cats.implicits._

object MapReduce {

  def foldMap[A, B: Monoid](seq: Vector[A])(f: A => B): B =
    seq.foldLeft(Monoid.empty[B])(_ |+| f(_))

}
