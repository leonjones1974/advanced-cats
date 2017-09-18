package uk.camsw.cats

import cats.Id

import scala.language.higherKinds

object IdMonad {
  def pure[A](v: A): Id[A] = v

  def map[A, B](fa: Id[A])(f: A => B) = f(fa)

  def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
}

