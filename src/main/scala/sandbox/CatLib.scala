package sandbox

import cats._
import cats.implicits._

object CatLib {

  final case class Cat(name: String, age: Int, color: String)

  implicit val catShow: Show[Cat] =
    Show.show(cat => s"${cat.name.show} is a ${cat.age.show} year-old ${cat.color.show} cat.")

  implicit val catEq: Eq[Cat] =
    Eq.apply { (cat1, cat2) =>
      cat1.name === cat2.name && cat1.age == cat2.age && cat1.color == cat2.color
    }
}

object CatTest extends App {

}