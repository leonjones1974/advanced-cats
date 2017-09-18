package uk.camsw.cats

import cats.Id
import cats.data.WriterT
import org.scalatest.{FunSpec, Matchers}

import scala.collection.immutable.IndexedSeq
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.language.postfixOps

class WriterTest extends FunSpec with Matchers {

  describe("Factorial") {
    it("should work") {
      Writer.factorial(10) shouldBe 3628800
    }

    it("can be run in parallel") {
      import ExecutionContext.Implicits.global

      val xs = for {
        n <- 1 to 2
      } yield Future{Writer.factorial(10)}

      Await.ready(Future.sequence(xs), 1 minute)
    }

    it("can use a writer to collate log entries") {
      Writer.factorialMapBoth(3).run shouldBe (Vector("fact 0 1", "fact 1 1", "fact 2 2", "fact 3 6"), 6)
    }

    it("log sequencing can then be maintained per thread") {
      import ExecutionContext.Implicits.global

      val xs = for {
        n <- 1 to 2
      } yield Future{Writer.factorialMapBoth(3)}

      val result = Await.result(Future.sequence(xs), 1 minute).toList
      result.map(_.run) shouldBe List(
        (Vector("fact 0 1", "fact 1 1", "fact 2 2", "fact 3 6"), 6),
        (Vector("fact 0 1", "fact 1 1", "fact 2 2", "fact 3 6"), 6)
      )
    }

    it("can be achieved using a for comprehension") {
      import ExecutionContext.Implicits.global

      println("c: " + Writer.factorialForComprehension(3))
      val xs = for {
        n <- 1 to 2
      } yield Future{Writer.factorialForComprehension(3)}

      val result = Await.result(Future.sequence(xs), 1 minute).toList
      result.map(_.run) shouldBe List(
        (Vector("fact 0 1", "fact 1 1", "fact 2 2", "fact 3 6"), 6),
        (Vector("fact 0 1", "fact 1 1", "fact 2 2", "fact 3 6"), 6)
      )
      println(s"result: $result")
    }

  }
}
