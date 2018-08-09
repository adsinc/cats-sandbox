package sandbox

import cats._
import cats.implicits._
import sandbox.CatLib.Cat

object CatLib {

  final case class Cat(name: String, age: Int, color: String)

  implicit val catShow: Show[Cat] =
    Show.show(cat =>
      s"${cat.name.show} is a ${cat.age.show} year-old ${cat.color.show} cat.")

  implicit val catEq: Eq[Cat] =
    Eq.apply { (cat1, cat2) =>
      cat1.name === cat2.name && cat1.age == cat2.age && cat1.color == cat2.color
    }
}

object CatTest extends App {
  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")

  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  println(cat1 === cat2)
  println(optionCat1 === Some(cat1))
  println(optionCat1 =!= optionCat2)
}
