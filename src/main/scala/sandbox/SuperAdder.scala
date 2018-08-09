package sandbox

import cats._
import cats.implicits._

object SuperAdder {

  def add[T: Monoid](items: List[T]): T =
    items.foldLeft(Monoid[T].empty)(_ |+| _)

  case class Order(totalCost: Double, quantity: Double)

  implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
    override def empty: Order = Order(0, 0)

    override def combine(x: Order, y: Order): Order = Order(
      x.totalCost |+| y.totalCost,
      x.quantity |+| y.quantity
    )
  }

}

object SuperAdderTest extends App {

  import SuperAdder._

  println(add(List(1, 2, 3)).show)
  println(add(List(Some(1), None, Some(3))).show)

  println(
    add(
      List(
        Order(10.1, 4),
        Order(8.22, 2.3)
      )))
}
