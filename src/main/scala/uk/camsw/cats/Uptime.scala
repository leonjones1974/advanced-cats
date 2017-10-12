package uk.camsw.cats

import cats.Applicative
import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.traverse._

import scala.concurrent.Future

trait UptimeClient[F[_]] {

  def getUptime(host: String)(implicit ap: Applicative[F]): F[Int] =
    ap.pure(uptime(host))

  val uptime = Map(
    "host1" -> 10,
    "host2" -> 5
  ).withDefault(_ => 0)
}

trait RealUptimeClient extends UptimeClient[Future] {

}

trait TestUptimeClient extends UptimeClient[cats.Id] {

}


case class UptimeService[F[_]](client: UptimeClient[F])(implicit ap: Applicative[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}