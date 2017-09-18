//import cats.{Functor, Monad}
//import cats.instances.list._
//import uk.camsw.cats.{IdMonad, Writer}
//import cats.syntax.either._
//val list1 = List(1, 2, 3)
//// list1: List[Int] = List(1, 2, 3)
//
//val list2 = Functor[List].map(list1)(_ * 2)
//// list2: List[Int] = List(2, 4, 6)
//
//
//val xs = Map("a" -> 1)
//
//Option(10).flatMap(_ => Option(4))
//
//IdMonad.pure(10)
//IdMonad.map(IdMonad.pure(10))(_ + 12)
//IdMonad.flatMap(IdMonad.pure(10))(_ => IdMonad.pure(30))
//Either.catchOnly[NumberFormatException]("foo".toInt)
//
//import uk.camsw.cats.Eval._
//Writer.factorial(10)

type Logged[A] = cats.data.Writer[Vector[String], A]
import cats.syntax.applicative._
import cats.instances.vector._
import cats.syntax.writer._

//val ans = 1.pure[Logged]
//val ans:Logged[Int] = 1.writer(Vector.empty[String])
//ans.flatMap(x => x.map(_ => Vector("and a message").tell)).run
1.pure[Logged].flatMap(ans => Vector("and a message").tell)

