package chapter3

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[A](head: A, tail: List[A]) extends List[A]

object List {

  private def foldRight[A, B](as: List[A], zero: B)(f: (A, B) => B): B = {
    as match {
      case Nil => zero
      case Cons(x, xs) => f(x, foldRight(xs, zero)(f))
    }
  }

  private def foldLeft[A, B](as: List[A], zero: B)(f: (A, B) => B): B = {
    @tailrec
    def loop(acc: B, list: List[A]): B = {
      list match {
        case Nil => acc
        case Cons(x, xs) => loop(f(x, acc), xs)
      }
    }
    loop(zero, as)
  }

  def tail[A](xs: List[A]): List[A] =
    xs match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }

  def drop[A](n: Int, xs: List[A]): List[A] = {
    def loop(n: Int, as: List[A]): List[A] = {
      if (n <= 0) as
      else loop(n - 1, tail(as))
    }

    loop(n, xs)
  }

  def dropWhile[A](xs: List[A], p: (A) => Boolean): List[A] = {
    xs match {
      case Nil => Nil
      case Cons(a, as) =>
        if (p(a)) dropWhile(as, p)
        else xs

    }
  }

  def setHead[A](h: A, xs: List[A]): List[A] =
    xs match {
      case Nil => Cons(h, Nil)
      case c: Cons[A] => Cons(h, c)
    }

  def sum(xs: List[Int]): Int = foldLeft(xs, 0)((a, b) => a + b)

  def product(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a: List[A], b: List[A]): List[A] = {
    a match {
      case Nil => b
      case Cons(x, xs) => Cons(x, append(xs, b))
    }
  }

  def init[A](as: List[A]): List[A] = {
    def loop(acc: List[A], list: List[A]): List[A] = {
      list match {
        case Nil | Cons(_, Nil) => acc
        case Cons(x, xs) => loop(append(acc, Cons(x, Nil)), xs)
      }
    }
    loop(Nil, as)
  }


}