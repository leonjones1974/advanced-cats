package uk.camsw.cats

import cats.instances.option._
import cats.syntax.eq._
import cats.syntax.show._
import org.scalatest.{FunSpec, Matchers}
import uk.camsw.cats.PrintableSyntax._
class CatTest extends FunSpec with Matchers {

  describe("A cat") {
    it("should have a format") {
      Cat("Leon", 43, "white").format shouldBe "Leon is a 43 year old white cat"
    }

    it("should belong to show") {
      Cat("Leon", 43, "white").show shouldBe "Leon is a 43 year old white cat"
    }

    it("should belong to eq") {
      val cat1 = Cat("Garfield", 35, "orange and black")
      val cat2 = Cat("Heathcliff", 30, "orange and black")
      cat1 =!= cat2 shouldBe true

      val optionCat1 = Option(cat1)
      val optionCat2 = Option.empty[Cat]
      optionCat1 =!= optionCat2 shouldBe true
    }
  }
}
