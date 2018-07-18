package sandbox

import cats._
import cats.implicits._
import sandbox.TreeFunctor.Tree.{branch, leaf}

object TreeFunctor {

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  object Tree {
    def leaf[A](value: A): Tree[A] = Leaf(value)

    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  }

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      fa match {
        case Leaf(value) => Leaf(f(value))
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      }
  }
}

object TreeFunctorTest extends App {

  import TreeFunctor._

  val tree: Tree[String] = branch(
    branch(leaf("string"), leaf("tree")),
    leaf("hello")
  )

  println(tree.map(s => s.toUpperCase))

  println(branch(leaf("string"), leaf("tree")).map(_.length))
}
