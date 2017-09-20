package uk.camsw.cats

import org.scalatest.{FunSpec, Matchers}

class ReadersTest extends FunSpec with Matchers {

  val db = Db(Map(1 -> "Leon"), Map("Leon" -> "pass"))

  describe("Reader") {
    it("can be used to resolve username") {
      Readers.findUsername(1).run(db) shouldBe Some("Leon")
    }

    it("can be used where the username does not exist") {
      Readers.findUsername(2).run(db) shouldBe None
    }

    it("can be used to check passwords - true") {
      Readers.checkPassword("Leon", "pass").run(db) shouldBe true
    }

    it("can be used to check passwords - wrong password") {
      Readers.checkPassword("Leon", "fish").run(db) shouldBe false
    }

    it("can be used to check passwords - wrong user") {
      Readers.checkPassword("Bob", "pass").run(db) shouldBe false
    }

    it("can check login - true") {
      Readers.checkLogin(1, "pass").run(db) shouldBe true
    }

    it("can check login - false") {
      Readers.checkLogin(1, "fish").run(db) shouldBe false
    }
  }
}
