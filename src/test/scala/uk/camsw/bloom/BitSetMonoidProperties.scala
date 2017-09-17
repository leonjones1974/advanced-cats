package uk.camsw.bloom

import uk.camsw.bloom.Implicits._

import scala.collection.immutable.BitSet


import cats.kernel.Monoid
import cats.implicits._
import com.roundeights.hasher.Algo
import com.roundeights.hasher.Algo._
import uk.camsw.bloom.HashFunction.boundedHash
import uk.camsw.bloom.Key.Key

import scala.collection.immutable.BitSet

/**
  * TODO: LJ - Cats appears to provide a test framework for the monoid laws.
  * Return to this and work out what that classpath needs to look like
  */
class BitSetMonoidProperties extends PropertyChecks {

  property("the zero identity is the empty bit set") {
    bitSetMonoid.empty shouldBe BitSet()
  }

  property("the combination of 2 bit sets should be a bitwise OR") {
    forAll(randomBitSet, randomBitSet) { (bs1, bs2) =>
      val combined = bs1 |+| bs2
      (bs1 subsetOf combined) && (bs2 subsetOf combined) shouldBe true
      combined -- bs1 -- bs2 shouldBe empty
    }
  }

  property("left identity + x == x") {
    forAll(randomBitSet) { (bs1) =>
      (BitSet() |+| bs1) shouldBe bs1
    }
  }
  property("x + right identity == x") {
    forAll(randomBitSet) { (bs1) =>
      (bs1 |+| BitSet()) shouldBe bs1
    }
  }

  property("the combination of bit sets should be associative") {
    forAll(randomBitSet, randomBitSet, randomBitSet) {
      (bs1, bs2, bs3) => {
        (bs1 |+|bs2 |+| bs3) shouldEqual (bs1 |+| (bs2 |+| bs3))
      }
    }
  }
}

