package uk.camsw.cats

import cats.Applicative
import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.traverse._

import scala.concurrent.Future

/**
  * In this example we have a client that reports the uptime of a 'remote host' and a service that 'sums' the
  * uptime across hosts
  * In the real-world we want to do this async, thus the client returns a Future[Int]
  * This is a ball-ache for testing though, so what we really want is to be able to abstract over that return type,
  * giving us a synchronous test version of the client and service
  *
  * 1. We abstract over the type constructor, rather than returning a Future[Int] we abstract to an F[_]
  * 2. Given an Applicative exists for F, we lift the result of the call to the remote host into that context,
  *   i.e. Future(uptime(host))
  * 3. We provide a test version that uses cats.Id rather than Future.  Id is a type alias Id[A] = A,
  * such that it provides a type-constructor for any type and an instance of Id is equivalent to the 'contained' value
  */
trait UptimeClient[F[_]] {

  /**
    * Here we lift the result of the uptime call into whatever applicative we decide F might be, i.e. a Future
    */
  def getUptime(host: String)(implicit ap: Applicative[F]): F[Int] =
    ap.pure(uptime(host))

  val uptime = Map(
    "host1" -> 10,
    "host2" -> 5
  ).withDefault(_ => 0)
}

/**
  * Our real client returns a Future
  */
trait RealUptimeClient extends UptimeClient[Future]

/**
  * Our fake client returns an Id, equivalent to simply returning the value
  */
trait TestUptimeClient extends UptimeClient[cats.Id]

/**
  * Then we can wrap our equivalently parameterized service around the client,
  * allowing us to inject a synchronous version should we wish to
  *
  * Note: The traverse is an abstraction that takes G[F[A]] and creates F[G[A]].  For example, rather than returning a:
  *   List[Future[Int]] which is what you would get with say a fold and which would require cumbersome unpacking, you get:
  *   Future[List[Int]], which in the case below we just sum, giving us a Future[Int]
  *
  * As mentioned, F may be Id, in which case we end up with Id[Int] (equivalent to just the Int)
  * Equally we could use Option or any other type that belongs to the Applicative type class
  */
case class UptimeService[F[_]](client: UptimeClient[F])(implicit ap: Applicative[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}


