package sandbox

import cats._
import cats.implicits._

object ProductImplement {
  def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
    for {
      a <- x
      b <- y
    } yield (a, b)
}
