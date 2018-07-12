package sandbox

import cats._
import cats.implicits._

object CatShow {

  final case class Cat(name: String, age: Int, color: String)

  implicit val catShow: Show[Cat] =
    Show.show(cat => s"${cat.name.show} is a ${cat.age.show} year-old ${cat.color.show} cat.")
}
