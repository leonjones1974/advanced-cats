package uk.camsw.cats

import org.scalatest.{FunSpec, FunSuite, Matchers}
import cats.instances.option._
class CartesiansTest extends FunSpec with Matchers {

  describe("Cartesian") {
    it("should support product") {
      Cartesians.product(Option(1), Option(2)) shouldBe Some(1 -> 2)
    }
  }

}
