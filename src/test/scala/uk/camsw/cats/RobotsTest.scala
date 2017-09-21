package uk.camsw.cats

import cats.data.EitherT
import org.scalatest.{FunSpec, Matchers}
import uk.camsw.cats.Robots.tacticalReport

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

class RobotsTest extends FunSpec with Matchers {

  def getLevelNested(bot: String)(implicit ec: ExecutionContext) = for {
    fpw <- Robots.getPowerLevel(bot)
  } yield {
    for {
      pwr <- fpw.right
    } yield pwr
  }

  import cats.instances.future._
  def getLevelNonNested(bot: String)(implicit ec: ExecutionContext) = for {
    fpw <- Robots.getPowerLevelT("Jazz")
  } yield fpw

  describe("robots") {
    it("should work using nested for comprehensions - happy") {
      Await.result(getLevelNested("Jazz"), 1 minute) shouldBe Right(6)
    }

    it("should work using nested for comprehensions - unhappy") {
      Await.result(getLevelNested("Boo"), 1 minute) shouldBe Left("unknown robot: Boo")
    }

    it("should work using non-nested for comprehensions - happy") {
      val x: EitherT[Future, String, Int] = for {
        pwr <- getLevelNonNested("Jazz")
      } yield pwr

      tacticalReport("Jazz", "Bumblebee") shouldBe "Jazz and Bumblebee need a recharge."

      tacticalReport("Bumblebee", "Hot Rod") shouldBe "Bumblebee and Hot Rod are ready to roll out!"
    }
  }
}
