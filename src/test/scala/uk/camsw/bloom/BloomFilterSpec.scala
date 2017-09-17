package uk.camsw.bloom

import uk.camsw.bloom.Key._

class BloomFilterSpec extends Spec {

  case class Employee(name: String, id: Int)
  val employee = Employee("Me", 43)
  implicit val id: Key[Employee] = _.id

  describe("Bloom Filter") {

    it("can be used with members of key") {
      aBloomFilter() + employee should possiblyContain(employee.id)
      aBloomFilter() + employee should possiblyContain(employee)
    }
  }
}
