package uk.camsw.cats

import cats.Foldable
import cats.kernel.Monoid
import org.scalatest.{FunSpec, FunSuite, Matchers}

class FoldTest extends FunSpec with Matchers {

  describe("fold") {
    it("should concat a list thus") {
      List(1, 2, 3).foldLeft(List.empty[Int])((acc, n) => n :: acc) shouldBe List(3, 2, 1)
      List(1, 2, 3).foldRight(List.empty[Int])(_ :: _) shouldBe List(1, 2, 3)
    }

    it("can implement map") {
      val f: Int => Int = _ * 2
      List(1, 2, 3).foldRight(List.empty[Int])((n, acc) => f(n) :: acc) shouldBe List(2, 4, 6)
    }

    it("can implement flatMap") {
      val f: Int => List[Int] = n => List(n, n + 1)
      List(1, 2, 3).foldRight(List.empty[Int])((n, acc) => f(n) ::: acc) shouldBe List(1, 2, 2, 3, 3, 4)
    }

    it("can implement filter") {
      val f: Int => Boolean = _ % 2 != 0
      List(1, 2, 3).foldRight(List.empty[Int])((n, acc) => if (f(n)) n :: acc else acc) shouldBe List(1, 3)
    }

    it("can implement sum") {
      List(1, 2, 3).foldRight(0)((n, acc) => acc + n) shouldBe 6
    }
  }

  describe("foldable") {
    import cats.instances.list._
    import cats.instances.stream._

    it("can sum too") {
      Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _) shouldBe 6
    }

    it("fold right is stack-safe") {
      intercept[StackOverflowError]{(0 to 10000000).toStream.foldRight(0)(_ + _)}
      Foldable[Stream].foldRight((0 to 1000000).toStream, cats.Eval.now(0))((a, b) => b.map(_ + a)).value shouldBe 1784293664
    }

    import cats.syntax.foldable._
    implicit val multMonoid: Monoid[Int] = new Monoid[Int] {
      override def empty: Int = 1

      override def combine(x: Int, y: Int): Int = x * y
    }

    it("can combine all using a monoid") {
      List(1, 2, 3, 4).combineAll shouldBe 24
    }

    it("can do a fold map") {
      List(1, 2, 3, 4).foldMap(_ * 2) shouldBe 384
    }

    it("can compose") {
      import cats.instances.vector._
      val ints = List(Vector(1, 2, 3), Vector(4, 5, 6))
      (Foldable[List] compose Foldable[Vector]).combineAll(ints) shouldBe 720
    }
  }


}
