package uk.camsw.bloom.temp

import org.scalatest.{FunSpec, FunSuite, Matchers}
import CodecInstances._
import Codec._

class CodecTest extends FunSpec with Matchers {

  describe("code") {
    it("should encode"){
      encode(123) shouldBe "123"
    }

    it("should decode"){
      decode[Int]("123") shouldBe Some(123)
    }

    it("should return none on decode failure") {
      decode[Int]("nan") shouldBe None
    }

    it("should support imap") {
      case class Box[A](value: A)
      implicit def boxCodec[A](implicit ev: Codec[A]): Codec[Box[A]] =
        ev.imap[Box[A]](Box(_), _.value)

      encode(Box(123)) shouldBe "123"
      decode[Box[Int]]("123") shouldBe Some(Box(123))
      decode[Box[Boolean]]("yes") shouldBe Some(Box(true))

    }
  }
}
