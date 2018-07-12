package sandbox

import cats._

object CatShow {

  final case class Cat(name: String, age: Int, color: String)

  implicit val catShow: Show[Cat] =
    Show.show(cat => s"${cat.name} is a ${cat.age} year-old ${cat.color} cat.")
}
