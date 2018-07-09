package sandbox

object PrintableLib {

  trait Printable[A] {
    def format(a: A): String
  }

  object Printable {
    def format[A](a: A)(implicit printable: Printable[A]): String =
      printable.format(a)
    def print[A](a: A)(implicit printable: Printable[A]): Unit =
      println(format(a))
  }

  object PrintableInstances {
    implicit val intPrintableInstance: Printable[Int] = a => a.toString
    implicit val stringPrintableInstance: Printable[String] = a => a.toString
    implicit val catPrintableInstance: Printable[Cat] = a => {
      val name = Printable.format(a.name)
      val age = Printable.format(a.age)
      val color = Printable.format(a.color)
      s"$name is a $age year-old $color cat."
    }
  }

  final case class Cat(name: String, age: Int, color: String)
}
