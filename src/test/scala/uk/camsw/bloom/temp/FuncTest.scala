package uk.camsw.bloom.temp

import cats._
import cats.implicits._
import org.scalatest.{FunSpec, Matchers}

import scala.language.higherKinds


class FuncTest extends FunSpec with Matchers {

  describe("Functors") {
    it("can lift a function to a context") {
      val f1 = (n: Int) => n * 2
      val lf1 = Functor[List].lift(f1)
      val of1 = Functor[Option].lift(f1)

      lf1(List(1, 2, 3)) shouldBe List(2, 4, 6)
      of1(Some(1)) shouldBe Some(2)
    }

    it("can be used to compose functions") {
      val f1 = (n: Int) => n * 2
      val f2 = (n: Int) => n + 0.5
      val f3 = f1.map(f2)

      f3(2) shouldBe 4.5
    }
  }
}
