package uk.camsw.bloom.temp

import cats.Monoid
import org.scalatest.{FunSpec, Matchers}
import cats.syntax.monoid._
class BoolTest extends FunSpec with Matchers {

  describe("boolean monoids") {
    it("could be a logical and") {
      implicit val boolAndMonoid: Monoid[Boolean] = new Monoid[Boolean] {
        override def empty: Boolean = true

        override def combine(x: Boolean, y: Boolean): Boolean = x && y
      }

      true |+| false shouldBe false
      false |+| true shouldBe false
      false |+| false shouldBe false
      true |+| true shouldBe true
    }

    it("could be a logical or") {
      implicit val boolOrMonoid: Monoid[Boolean] = new Monoid[Boolean] {
        override def empty: Boolean = false

        override def combine(x: Boolean, y: Boolean): Boolean = x || y
      }

      true |+| false shouldBe true
      false |+| true shouldBe true
      false |+| false shouldBe false
      true |+| true shouldBe true
    }

    it("could be a logical xor") {
      implicit val boolXorMonoid: Monoid[Boolean] = new Monoid[Boolean] {
        override def empty: Boolean = false

        override def combine(x: Boolean, y: Boolean): Boolean = x ^ y
      }

      true |+| false shouldBe true
      false |+| true shouldBe true
      false |+| false shouldBe false
      true |+| true shouldBe false
    }
  }
}
