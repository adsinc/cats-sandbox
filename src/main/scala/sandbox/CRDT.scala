package sandbox

import cats.Monoid

object CRDT {

  final case class GCounter(counters: Map[String, Int]) {
    def increment(machine: String, amount: Int): GCounter = {
      val newAmount = amount + counters.getOrElse(machine, 0)
      GCounter(counters + (machine -> newAmount))
    }

    def merge(that: GCounter): GCounter =
      GCounter(that.counters ++ counters.map {
        case (k, v) =>
          k -> math.max(v, counters.getOrElse(k, 0))
      })

    def total: Int =
      counters.values.sum
  }

  trait BoundedSemiLattice[A] extends Monoid[A] {
    def combine(x: A, y: A): A

    def empty: A
  }

  implicit val intBoundedSemiLattice: BoundedSemiLattice[Int] =
    new BoundedSemiLattice[Int] {
      def combine(x: Int, y: Int): Int =
        x max y

      def empty: Int =
        0
    }

  implicit def setBoundedSemiLattice[T](): BoundedSemiLattice[Set[T]] =
    new BoundedSemiLattice[Set[T]] {
      def combine(x: Set[T], y: Set[T]): Set[T] =
        x union y

      def empty: Set[T] =
        Set.empty[T]
    }

}
