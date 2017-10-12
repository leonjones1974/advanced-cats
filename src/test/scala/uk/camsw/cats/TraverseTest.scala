package uk.camsw.cats

import javax.xml.crypto.dsig.Transform

import cats.Applicative
import org.scalatest.{FunSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class TraverseTest extends FunSpec with Matchers {
  val hostnames = List("alpha.example.com", "beta.example.com", "gamma.demo.com")

  def getUptime(hostname: String): Future[Int] = Future(hostname.length * 60)

  import cats.instances.future._
  import cats.syntax.cartesian._
  import cats.syntax.applicative._
  import cats.instances.vector._
  import cats.instances.list._
  import cats.instances.option._

  describe("applicative combine") {
    it("can combine") {

      def listTraverse[F[_] : Applicative, A, B](xs: List[A])(f: A => F[B]): F[List[B]] = {
        xs.foldLeft(List.empty[B].pure[F])((acc, a) => (acc |@| f(a)).map(_ :+ _))
      }

      def listSequence[F[_] : Applicative, B](xs: List[F[B]]): F[List[B]] =
        listTraverse(xs)(identity)

      Await.result(
        listTraverse(hostnames)(getUptime),
        1.second) shouldBe List(1020, 960, 840)


      (List(1, 2) |@| List(3, 4)).map((_, _)) shouldBe List((1, 3), (1, 4), (2, 3), (2, 4))
      listSequence(List(Vector(1, 2), Vector(3, 4))) shouldBe Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))
      (Option(1) |@| Option(2)).map(_ + _) shouldBe Some(3)
      (Option(1) |@| Option.empty[Int]).map(_ + _) shouldBe None
      listSequence(List(Option(1), Option(2))) shouldBe Some(List(1, 2))
      listTraverse[Option, Int, Int](List(1, 2, 3))(n => if(n > 3) Option.empty[Int] else Option(n)) shouldBe Option(List(1, 2, 3))
    }

    it("is easier using cats") {
      cats.Traverse[List].traverse(List(1, 2, 3, 4))(Option(_)) shouldBe Some(List(1, 2, 3, 4))
      val f = cats.Traverse[List].traverse(hostnames)(getUptime)
      Await.result(
        f,
        1.second) shouldBe List(1020, 960, 840)

    }
  }
}
