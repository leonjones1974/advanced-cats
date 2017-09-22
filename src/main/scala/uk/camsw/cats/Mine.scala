package uk.camsw.cats

case class Mine[A](v: A) {

  def map[B](f: A => B): Mine[B] = {
    val x = copy(f(v))
    println(s"called map for: $v -> $x`")
    x
  }

  def flatMap[B](f: A => Mine[B]): Mine[B] = {
    val x = f(v)
    println(s"called flatmap for: $v -> $x")
    x
  }
}
