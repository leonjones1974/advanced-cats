package uk.camsw.cats

trait Printable[A] {

  def format(x: A): String

  def contramap[B](func: B => A): Printable[B] = {
    val self = this
    new Printable[B] {
      override def format(b: B): String = self.format(func(b))
    }
  }
}

object PrintableInstances {
  implicit val stringPrintable = new Printable[String] {
    override def format(x: String): String = "\"" + x + "\""
  }

  implicit val intPrintable = new Printable[Int] {
    override def format(x: Int): String = x.toString
  }

  implicit val booleanPrintable = new Printable[Boolean] {
    def format(value: Boolean): String =
      if (value) "yes" else "no"
  }
}

object Printable {

  def format[A](x: A)(implicit ev: Printable[A]): String = ev.format(x)

  def print[A](x: A)(implicit ev: Printable[A]): Unit =
    println(format(x))
}

object PrintableSyntax {

  implicit class PrintOps[A](x: A) {
    def format(implicit ev: Printable[A]): String = Printable.format(x)

    def print(implicit ev: Printable[A]): Unit = Printable.print(x)
  }

}
