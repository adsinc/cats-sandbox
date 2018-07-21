package sandbox

import cats._

object Identity {

  def pure[T](value: T): Id[T] = value

  def map[T, V](t: Id[T])(f: T => V): Id[V] = f(t)

  def flatMap[T, V](t: Id[T])(f: T => Id[V]): Id[V] = f(t)
}

object TestIdentity extends App {
  import Identity._

  val id1 = pure(1)
  val id2 = pure(2)
  println(map(id1)(_ + 10))
  println(flatMap(id1)(a => map(id2)(b => a + b)))
}