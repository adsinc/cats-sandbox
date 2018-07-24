package sandbox

import cats.data.State
import sandbox.PostOrderCalculator._

object PostOrderCalculator {
  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => op(_ + _)
      case "-" => op(_ - _)
      case "*" => op(_ * _)
      case "/" => op(_ / _)
      case n => State(s => (n.toInt :: s, n.toInt))
    }

  def op(fn: (Int, Int) => Int): CalcState[Int] =
    State { s =>
      val a :: b :: newState = s
      val value = fn(a, b)
      (value :: newState, fn(a, b))
    }

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(State.pure[List[Int], Int](0)) {
      (state, sym) => state.flatMap(_ => evalOne(sym))
    }

  def evalInput(s: String): Int =
    evalAll(s.split(" ").toList).runA(Nil).value

}

object PostOrderCalculatorRunner extends App {
  val program1 = evalAll(List("1", "2", "+", "3", "*"))
  println(program1.run(Nil).value)

  val program2 = for {
    _ <- evalAll(List("1", "2", "+"))
    _ <- evalAll(List("3", "4", "+"))
    ans <- evalOne("*")
  } yield ans

  println(program2.run(Nil).value)

  println(evalInput("1 2 + 3 4 + *"))
}
