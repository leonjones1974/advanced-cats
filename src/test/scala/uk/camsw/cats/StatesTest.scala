package uk.camsw.cats

import org.scalatest.{FunSpec, FunSuite, Matchers}
import uk.camsw.cats.States.{evalAll, evalOne}

class StatesTest extends FunSpec with Matchers {

  describe("State") {
    it("should evaluate an operand") {
      evalOne("2").runA(Nil).value shouldBe 2
    }

    it("should perform an postfix operation") {
      val calc = for {
        _ <- evalOne("2")
        _ <- evalOne("6")
        ans <- evalOne("*")
      } yield ans

      calc.runA(Nil).value shouldBe 12
    }

    it("should evaluate a list") {
      val input = List("1", "2", "+", "3", "*")
      evalAll(input).runA(Nil).value shouldBe 9
    }
  }
}
