package uk.camsw.bloom.temp

import cats.kernel.Monoid
import cats.syntax.semigroup._
/**
  * Created by leonjones on 12/09/17.
  */
object SuperAdder {

  def add[A](items: List[A])(implicit M: Monoid[A]): A = items.foldLeft(M.empty)(M.combine)
}
