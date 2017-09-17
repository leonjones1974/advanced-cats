package uk.camsw.cats

import scala.util.Try

trait Codec[A] {
  def encode(value: A): String

  def decode(value: String): Option[A]

  def imap[B](dec: A => B, enc: B => A): Codec[B] = {
    val self = this
    new Codec[B] {
      override def encode(value: B): String = self.encode(enc(value))

      override def decode(value: String): Option[B] = self.decode(value).map(dec)
    }
  }
}

object CodecInstances {
  implicit val intCodec: Codec[Int] = new Codec[Int] {
    override def encode(value: Int) = value.toString

    override def decode(value: String) = Try {
      value.toInt
    }.toOption
  }

  implicit val boolCodec: Codec[Boolean] = new Codec[Boolean] {
    override def encode(value: Boolean): String = if (value) "yes" else "no"

    override def decode(value: String): Option[Boolean] = value match {
      case "yes" => Some(true)
      case "no" => Some(false)
      case _ => None
    }
  }

}

object Codec {
  def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): Option[A] = c.decode(value)

}