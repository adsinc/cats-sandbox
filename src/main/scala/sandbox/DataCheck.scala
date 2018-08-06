package sandbox

object DataCheck {

  trait Check[E, A] {
    def apply(value: A): Either[E, A]

    def and(that: Check[E, A]): Check[E, A] = ???
  }

}
