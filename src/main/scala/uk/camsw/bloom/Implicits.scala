package uk.camsw.bloom

import cats.kernel.Monoid

import scala.collection.immutable.BitSet

object Implicits {

  implicit val bitSetMonoid: Monoid[BitSet] = new Monoid[BitSet] {
    val empty: BitSet = BitSet()

    override def combine(x: BitSet, y: BitSet): BitSet = x | y
  }

}
