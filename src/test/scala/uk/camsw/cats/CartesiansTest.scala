package uk.camsw.cats

import cats.Applicative
import cats.instances.option._
import org.scalatest.{FunSpec, Matchers}

import scala.collection.immutable
import scala.concurrent.Future

class CartesiansTest extends FunSpec with Matchers {

  describe("Cartesian") {
    it("should support product") {
      Cartesians.product(Option(1), Option(2)) shouldBe Some(1 -> 2)
    }

    it("can combine as a tuple") {
      import cats.instances.option._
      import cats.syntax.cartesian._
      val x: Option[Int] = Option(1)
      val y: Option[Int] = Option(2)
      (x |@| y).tupled shouldBe Some(1, 2)
    }

    val f = (a1: Int, b1: Int) => "hello" + a1 + b1
    it("can be combined with a function lifted into the context") {
      import cats.instances.option._
      import cats.syntax.cartesian._
      val x: Option[Int] = Option(1)
      val y: Option[Int] = Option(2)
      val r = (x |@| y).apWith[String](Some(f))
      r shouldBe Some("hello12")
    }


    it("can be combined") {
      import cats.instances.list._
      val c = Applicative[List].compose[Option]

      (for {
        x <- (21 to 23).toList
        y <- c.pure(x)
      } yield y) shouldBe List(Some(21), Some(22), Some(23))


      val r = Applicative[List].compose[Option].pure(1)
      r shouldBe List(Some(1))
    }
  }
}
