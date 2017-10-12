package uk.camsw.cats

import org.scalatest.{FunSpec, Matchers}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import cats.instances.future._

class UptimeClientTest extends FunSpec with Matchers {

  describe("Abstraction over type constructor") {
    it("should allow me to get a future") {
      val c = new RealUptimeClient {}
      val f: Future[Int] = c.getUptime("host1")
      Await.result(f, 1 second) shouldBe 10
    }

    it("should allow me to get a real value") {
      val c = new TestUptimeClient {}
      c.getUptime("host1") shouldBe 10
    }


    it("should be a useful abstraction across both") {
      import cats.syntax.flatMap._

      val tup: Future[(Int, Int)] = for {
        y <- new TestUptimeClient{}.getUptime("host1")
        b <- new RealUptimeClient {}.getUptime("host2")
      } yield (y, b)

      Await.result(tup, 1 second) shouldBe ((10, 5))
    }

  }

  describe("Abstraction over monads") {
    it("should allow us to use a test client") {
      val c = new TestUptimeClient {}
      UptimeService(c).getTotalUptime(List("host1", "host2")) shouldBe 15
    }

    it("should allow us to use a real client") {
      val c = new RealUptimeClient {}
      Await.result(UptimeService[Future](c).getTotalUptime(List("host1", "host2")), 1 second) shouldBe 15
    }
  }
}
