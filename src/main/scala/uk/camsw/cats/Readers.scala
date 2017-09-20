package uk.camsw.cats

import cats.data.Reader

case class Db(users: Map[Int, String], passwords: Map[String, String])

import cats.syntax.applicative._

object Readers {
  type DbReader[A] = Reader[Db, A]


  def findUsername(id: Int): DbReader[Option[String]] =
    Reader[Db, Option[String]](_.users.get(id))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader[Db, Boolean](_.passwords.get(username).contains(password))

  def checkLogin(id: Int, password: String): DbReader[Boolean] = {
    for {
      u <- findUsername(id)
      c <- u.map(name => checkPassword(name, password)).getOrElse(false.pure[DbReader])
    } yield c
  }

}
