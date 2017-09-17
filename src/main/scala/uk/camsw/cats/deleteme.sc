import cats.Functor
import cats.instances.list._
import cats.instances.option._

val list1 = List(1, 2, 3)
// list1: List[Int] = List(1, 2, 3)

val list2 = Functor[List].map(list1)(_ * 2)
// list2: List[Int] = List(2, 4, 6)


val xs = Map("a" -> 1)

List(1,2).flatMap(List.apply)