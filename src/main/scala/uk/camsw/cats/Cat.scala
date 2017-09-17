package uk.camsw.cats

import cats.{Eq, Show}

case class Cat(name: String, age: Int, color: String)

object Cat {

  implicit val catPrintable = new Printable[Cat] {
    override def format(x: Cat): String = s"${x.name} is a ${x.age} year old ${x.color} cat"
  }

  implicit val catShow: Show[Cat] = Show.show(x => s"${x.name} is a ${x.age} year old ${x.color} cat")

  implicit val catEq: Eq[Cat] = Eq.instance(_ == _)
}
