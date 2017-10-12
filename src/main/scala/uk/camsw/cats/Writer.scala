package uk.camsw.cats

import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._

object Writer {

  def slowly[A](body: => A) = try body finally Thread.sleep(100)

  def factorial(n: Int): Int = slowly {
    n match {
      case 0 => 1
      case y => y * factorial(y - 1)
    }
  }

  type Logged[A] = cats.data.Writer[Vector[String], A]

  def factorialMapBoth(n: Int): cats.data.Writer[Vector[String], Int] = slowly {
    n match {
      case 0 => 1.pure[Logged].mapWritten(_ :+ "fact 0 1")
      case y => factorialMapBoth(y - 1).mapBoth {
        case (log, value) =>
          val y1 = value * y
          (log :+ s"fact $y $y1", y1)
      }
    }
  }

  // todo: I don't understand how the final map is working
  def factorialForComprehension(n: Int): Logged[Int] = slowly {
    for {
      ans <- if (n == 0) 1.pure[Logged] else factorialForComprehension(n - 1).map(_ * n)
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  }

}
