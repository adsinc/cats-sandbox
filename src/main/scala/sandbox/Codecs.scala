package sandbox

object Codecs {

  trait Codec[A] {
    self =>

    def encode(value: A): String

    def decode(value: String): A

    def imap[B](dec: A => B, enc: B => A): Codec[B] =
      new Codec[B] {
        override def encode(value: B): String = self.encode(enc(value))

        override def decode(value: String): B = dec(self.decode(value))
      }
  }

  def encode[A](value: A)(implicit c: Codec[A]): String =
    c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): A =
    c.decode(value)

  implicit val stringCodec: Codec[String] =
    new Codec[String] {
      override def encode(value: String): String = value

      override def decode(value: String): String = value
    }

  implicit val doubleCodec: Codec[Double] =
    stringCodec.imap(_.toDouble, _.toString)

  final case class Box[A](value: A)

  implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] =
    c.imap(Box(_), _.value)
}

object CodecsTest extends App {

  import Codecs._

  println(encode(1.2))
  println(decode[Double]("1.2"))

  println(encode(Box(1.2)))
  println(decode[Box[Double]]("1.2"))
}
