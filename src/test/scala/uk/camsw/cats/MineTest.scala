package uk.camsw.cats

import org.scalatest.FunSpec

class MineTest extends FunSpec {

  describe("hand crafted monad") {
    it("should be usable in a for-comprehension") {
      for {
        a <- Mine("hello")
        b <- Mine("World")
      } yield {
        a + b
      }

      Mine("hello").flatMap(a => Mine("World").map(b => a + b))
    }
  }
}
