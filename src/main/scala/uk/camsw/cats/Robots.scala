package uk.camsw.cats


import scala.concurrent.duration._
import cats.data.EitherT
import cats.instances.future._

import scala.concurrent.{Await, ExecutionContext, Future}

object Robots {

  type Response[A] = Future[Either[String, A]]

  type ResponseT[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def getPowerLevel(bot: String)(implicit ec: ExecutionContext): Response[Int] =
    Future {
      powerLevels.get(bot) match {
        case None => Left(s"unknown robot: $bot")
        case Some(pw) => Right(pw)
      }
    }

  def getPowerLevelT(bot: String)(implicit ec: ExecutionContext): ResponseT[Int] =
    EitherT[Future, String, Int](getPowerLevel(bot))

  def canSpecialMove(ally1: String,
                     ally2: String)(implicit ec: ExecutionContext): ResponseT[Boolean] = for {
    power1 <- getPowerLevelT(ally1)
    power2 <- getPowerLevelT(ally2)
  } yield (power1 + power2) > 15

  def tacticalReport(ally1: String, ally2: String)(implicit ec: ExecutionContext): String =
    Await.result(canSpecialMove(ally1, ally1).value, 1 second) match {
      case Left(msg) => s"Comms error: $msg"
      case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) => s"$ally1 and $ally2 need a recharge."
    }
}
