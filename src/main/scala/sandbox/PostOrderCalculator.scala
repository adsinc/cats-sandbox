package sandbox

import cats.data.State

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
      (newState, fn(a, b))
    }

  def evalAll() = ???

}

object PostOrderCalculatorRunner extends App {

}
