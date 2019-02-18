package edu.depauw.bankocr.v2

object Util {
  def traverse[A, B](list: List[A])(f: A => Option[B]): Option[List[B]] =
    list.foldRight[Option[List[B]]](Some(List())) {
      case (_, None)     => None
      case (a, Some(bs)) => f(a).map(_ :: bs)
    }

  def dot(as: List[Int], bs: List[Int]): Int = (as zip bs).map {
    case (a, b) => a * b
  }.sum

  def updateIndex[A](list: List[A], i: Int)(f: A => A): List[A] =
    list.take(i) ::: List(f(list(i))) ::: list.drop(i + 1)
}