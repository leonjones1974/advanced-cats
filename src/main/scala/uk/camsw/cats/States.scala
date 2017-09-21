package uk.camsw.cats

import cats.data.State

object States {
  type CalcState[A] = State[List[Int], A]

  def operand(n: Int): CalcState[Int] =
    State[List[Int], Int](
      stack => (n :: stack, n)
    )

  def operation(f: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int](stack => {
      val r = f(stack.head, stack.tail.head)
      (r :: stack.drop(2), r)
    })


  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" =>
        operation(_ + _)
      case "*" =>
        operation(_ * _)
      case n =>
        operand(n.toInt)
    }

  import cats.syntax.applicative._
  def evalAll(xs: List[String]): CalcState[Int] =
    xs.foldLeft(0.pure[CalcState])((a, b) => a.flatMap(_ => evalOne(b)))

}
