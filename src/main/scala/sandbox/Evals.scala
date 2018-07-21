package sandbox

import cats.Eval

object Evals {
  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(foldRight(tail, acc)(fn).map(fn(head, _)))
      case Nil =>
        Eval.now(acc)
    }
}

object EvalTest extends App {

  val to = 100000

  println(Evals.foldRight((1 to to).toList, 0)(_ + _).value)

  println((1 to to).toList.sum)
}