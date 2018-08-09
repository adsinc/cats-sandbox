package sandbox

object ContramapPrintable extends App {

  trait Printable[A] {
    def format(value: A): String

    def contramap[B](func: B => A): Printable[B] =
      (value: B) => format(func(value))
  }

  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  implicit val stringPrintable: Printable[String] =
    value => s""""$value""""

  implicit val booleanPrintable: Printable[Boolean] =
    value => if (value) "yes" else "no"

  println(format("hello"))
  println(format(true))

  final case class Box[A](value: A)

  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
    p.contramap(_.value)

  println(format(Box("hello")))
  println(format(Box(true)))

}
