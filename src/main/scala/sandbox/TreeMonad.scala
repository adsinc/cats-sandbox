package sandbox

import cats._
import cats.implicits._

object TreeMonad {

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)

  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    override def pure[A](x: A): Tree[A] = leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] =
      fa match {
        case Branch(l, r) => branch(flatMap(l)(f), flatMap(r)(f))
        case Leaf(a)      => f(a)
      }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
      f(a) match {
        case Branch(leftTree, rightTree) =>
          branch(
            flatMap(leftTree) {
              case Left(v)  => tailRecM(v)(f)
              case Right(v) => pure(v)
            },
            flatMap(rightTree) {
              case Left(v)  => tailRecM(v)(f)
              case Right(v) => pure(v)
            }
          )
        case Leaf(Left(value)) =>
          tailRecM(value)(f)
        case Leaf(Right(value)) =>
          leaf(value)
      }
  }
}

object TreeMonadTest extends App {

  import TreeMonad._

  val tree = branch(leaf(1), leaf(2))

  val r = for {
    t <- tree
  } yield t * 2

  println(r)
}
