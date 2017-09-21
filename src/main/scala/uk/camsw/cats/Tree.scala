package uk.camsw.cats

import cats.{Functor, Monad, Show}

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object TreeInstances {

  implicit def treeShow[A](implicit ev: Show[A]): Show[Tree[A]] = Show.show {
    case Leaf(v) => ev.show(v)
    case Branch(l: Tree[A], r: Tree[A]) => s"(${treeShow[A].show(l)},${treeShow[A].show(r)})"
  }

  def leaf[A](v: A): Tree[A] = Leaf(v)

  def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: (A) => B): Tree[B] = fa match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l: Tree[A], r: Tree[A]) => Branch(map(l)(f), map(r)(f))
    }
  }

  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    override def flatMap[A, B](fa: Tree[A])(f: (A) => Tree[B]): Tree[B] = fa match {
      case Leaf(x) => f(x)
      case Branch(l, r) =>
        branch(flatMap(l)(f), flatMap(r)(f))
    }

    override def tailRecM[A, B](a: A)(f: (A) => Tree[Either[A, B]]): Tree[B] = f(a) match {
      case Leaf(Left(a1)) => tailRecM(a1)(f)
      case Leaf(Right(b1)) => pure(b1)
      case Branch(l, r) =>
        branch(
          flatMap(l) {
            case Left(a1) => tailRecM(a1)(f)
            case Right(b1) => pure(b1)
          },
          flatMap(r) {
            case Left(b1) => tailRecM(b1)(f)
            case Right(b1) => pure(b1)
          }
        )
    }

    override def pure[A](x: A): Tree[A] = leaf(x)
  }
}