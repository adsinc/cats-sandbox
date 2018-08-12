package sandbox

import cats._
import cats.implicits._

object CRDT {

  final case class GCounter[A](counters: Map[String, A]) {
    def increment(machine: String, amount: A)(implicit m: Monoid[A]): GCounter[A] = {
      val newAmount = amount |+| counters.getOrElse(machine, m.empty)
      GCounter(counters + (machine -> newAmount))
    }

    def merge(that: GCounter[A])(implicit b: BoundedSemiLattice[A]): GCounter[A] =
      GCounter(counters |+| that.counters)

    def total(implicit m: Monoid[A]): A =
      counters.values.toList.combineAll
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
