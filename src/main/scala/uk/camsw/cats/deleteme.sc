import cats.data.State
import uk.camsw.cats.States

States.operand(10).run(List.empty).value


val a = State[Int, String] {
  state => (state, s"the state is $state")
}

a.run(10).value
a.runS(10).value
a.runA(10).value

val getDemo = State.get[Int]
getDemo.run(10).value

val setDemo = State.set[Int](30)
setDemo.run(10).value

val pureDemo = State.pure[Int, String]("Result")
pureDemo.run(10).value

val inspectDemo = State.inspect[Int, String](_ + "!")
inspectDemo.run(20).value

val modifyDemo = State.modify[Int](_ * 2)
modifyDemo.run(20).value

import State._
val program: State[Int, (Int, Int, Int)] = for {
  a <- get[Int]
  _ <- set[Int](a + 1)
  b <- get[Int]
  _ <- modify[Int](_ + 1)
  c <- inspect[Int, Int](_ * 1000)
} yield (a, b, c)

program.run(10).value
