package uk.camsw.cats

import cats.instances.int._
import cats.instances.string._
import cats.syntax.functor._
import cats.syntax.show._
import org.scalatest.{FunSpec, Matchers}
import uk.camsw.cats.TreeInstances._

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
      val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))): Tree[Int]
      tree.map(_ * 2).show shouldBe "((2,4),(6,8))"
    }

    it("has a monad instance - pure") {
      import cats.syntax.applicative._
      1.pure[Tree] shouldBe leaf(1)
    }

    it("has a monad instance - flatmap") {
      import TreeInstances.treeMonad
      import cats.syntax.flatMap._

      val r = branch(leaf(100), leaf(200)).flatMap(x => branch(leaf(x - 1), leaf(x + 1)))

      r shouldBe branch(branch(leaf(99), leaf(101)), branch(leaf(199), leaf(201)))

    }
  }

  //    it("has a monad instance - for comprehension") {
  //      for {
  //        a <- branch(leaf(100), leaf(200))
  //        b <- branch(leaf(a - 10), leaf(a + 10))
  //        c <- branch(leaf(b - 1), leaf(b + 1))
  //      } yield c
  //    }
//}

}
