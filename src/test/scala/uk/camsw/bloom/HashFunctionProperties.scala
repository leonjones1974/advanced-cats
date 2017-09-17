package uk.camsw.bloom

import com.roundeights.hasher.Algo
import uk.camsw.bloom.HashFunction._

class HashFunctionProperties extends PropertyChecks {

  property("bound transforms any integer to be within bounds") {
    forAll { (n: Int, r: Range) =>
      bound(r)(n) should (be >= r.min and be <= r.max)
    }
  }

  property("for any bounded hashing function the int hash should be within the given range") {
    forAll() { (algo: Algo, s: String) =>
      boundedHash(algo, 0 to 999)(s) should (be >= 0 and be <= 999)
    }
  }

  // TODO: This is just an arbitrary sanity check for now.  Test properly using a recognised algorithm/ test framework and put a lot more effort into the hashing function
  property("collision rate should be less than 1% where buffer is 2 orders larger than entry list") {
    forAll() { (algo: Algo) =>
      val entries = 10000
      val buffer = entries * 10 * 10
      val hash = boundedHash(algo, 0 to buffer)
      val hashes = for {
        n: Int <- (1 to entries).toSet
      } yield hash(random.alphanumeric.take(10).toList.mkString)

      val collisions = (entries.toDouble - hashes.size) / entries
      collisions should (be <= 0.01)
    }
  }


}
