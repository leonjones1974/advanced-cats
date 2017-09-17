package uk.camsw.bloom.temp

import org.scalatest.{FunSpec, Matchers}
import PrintableSyntax._
import PrintableInstances._
import uk.camsw.bloom.temp.Printable.{format, print}

class PrintableTest extends FunSpec with Matchers {

  describe("Printable") {
    it("should format an int") {
      format(123) shouldBe "123"
      print(123)
    }

    it("should have a syntax") {
      123.format shouldBe "123"
      123.print
    }

    it("should be able to print a boolean") {
      true.format shouldBe "yes"
      false.format shouldBe "no"
    }

    it("should be able to contramap") {
      final case class Box[A](value: A)

      implicit def boxPrintable[A](implicit ev: Printable[A]) = ev.contramap[Box[A]](_.value)

      format(Box("Hello")) shouldBe "\"Hello\""
      format(Box(true)) shouldBe "yes"
    }
  }
}
