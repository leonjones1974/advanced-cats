package uk.camsw.cats

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._

import scala.language.higherKinds

object Cartesians {

  def product[M[_] : Monad, A, B](fa: M[A],
                                  fb: M[B]
                                 ): M[(A, B)] = {
    for {
      a <- fa
      b <- fb
    } yield (a, b)
  }

  case class USer(name: String, age: Int)

}
