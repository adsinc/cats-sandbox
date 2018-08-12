package sandbox

import cats._
import cats.implicits._

object CRDT {

  trait GCounter[F[_, _], K, V] {
    def increment(f: F[K, V])(k: K, v: V)(implicit m: Monoid[V]): F[K, V]

    def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]

    def total(f1: F[K, V])(implicit m: Monoid[V]): V
  }

  object GCounter {
    def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]): GCounter[F, K, V] = counter
  }

  implicit def mapGCounter[K, V]: GCounter[Map, K, V] =
    new GCounter[Map, K, V] {
      def increment(map: Map[K, V])(k: K, v: V)(implicit m: Monoid[V]): Map[K, V] = {
        val newV = v |+| map.getOrElse(k, m.empty)
        map + (k -> newV)
      }

      def merge(f1: Map[K, V], f2: Map[K, V])(implicit b: BoundedSemiLattice[V]): Map[K, V] =
        f1 |+| f2

      def total(f1: Map[K, V])(implicit m: Monoid[V]): V =
        f1.values.toList.combineAll
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

  trait KeyValueStore[F[_, _]] {
    def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

    def get[K, V](f: F[K, V])(k: K): Option[V]

    def getOrElse[K, V](f: F[K, V])(k: K, default: V): V =
      get(f)(k).getOrElse(default)

    def values[K, V](f: F[K, V]): List[V]
  }
}
