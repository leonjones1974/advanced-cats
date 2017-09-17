package uk.camsw.bloom.temp

import cats.instances.int._
import cats.instances.string._
import cats.syntax.functor._
import cats.syntax.show._
import org.scalatest.{FunSpec, Matchers}
import uk.camsw.bloom.temp.TreeInstances._

class TreeTest extends FunSpec with Matchers {

  describe("tree") {
    it("is possible to map a leaf") {
      leaf(10).map(_ + 10) shouldBe Leaf(20)
    }

    it("is possible to show a leaf") {
      leaf(10).show shouldBe "10"
    }

    it("is possible to show a branch") {
      val left = Leaf("left")
      val right = Leaf("right")
      branch(left, right).show shouldBe "(left,right)"
    }

    it("is possible to map a branch") {
      val left = Leaf("Left")
      val right = Leaf("Right")
      branch(left, right).map(_.toUpperCase).show shouldBe "(LEFT,RIGHT)"
    }

    it("is possible to show a complex tree") {
      branch(branch(leaf(1), leaf(2)), branch(leaf(3), leaf(4))).show shouldBe "((1,2),(3,4))"
    }

    it("is possible to map a complex tree") {
      //     *
      //   *   *
      //  1 2 3 4
      val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))) : Tree[Int]
      tree.map(_ * 2).show shouldBe "((2,4),(6,8))"
    }
  }
}
