package uk.camsw.bloom.temp

import cats.Monoid
import org.scalatest.{FunSpec, FunSuite, Matchers}
import cats.instances.int._
import cats.instances.option._
/**
  * Created by leonjones on 12/09/17.
  */
class SuperAdderTest extends FunSpec with Matchers {

  describe("Super adders") {
    it("should add numbers") {
      SuperAdder.add(List(1, 2, 3, 4)) shouldBe 10
    }

    it("should add options") {
      SuperAdder.add(List(Option(1), Option(2), Option(3), Option(4))) shouldBe Some(10)
    }

    it("should add orders") {
      case class Order(totalCost: Double, quantity: Double)
      implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
        override def empty: Order = Order(0, 0)

        override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
      }
      SuperAdder.add(List(Order(100, 2), Order(50, 10))) shouldBe Order(150, 12)
    }
  }
}
