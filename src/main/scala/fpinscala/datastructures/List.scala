package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def productFoldRight(ds: List[Double]): Double = foldRight(ds, 1.0)(_ * _)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(h, t)
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, b) => b + 1)

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n -1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def sumLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def productLeft(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))

  def sum2(ns: List[Int]): Int = foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

  def add1(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(h + 1, add1(t))
  }

  def doubleToString(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(h.toString, doubleToString(t))
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((a: A, b: List[B]) => Cons(f(a), b))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])((a, b) => if(f(a)) Cons(a, b) else b)

  def flatMap2[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((a: A, b: List[B]) => append(f(a), b))

  def appendLeft[A](l0: List[A], l1: List[A]): List[A] = foldLeft(reverse(l0), l1)((b, a) => Cons(a, b))

  def appendRight[A](l0: List[A], l1: List[A]): List[A] = foldRight(l0, l1)((a, b) => Cons(a, b))

  def concatenate[A](ll: List[List[A]]): List[A] =
    foldRight(ll, Nil: List[A])(append)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concatenate(map(as)(f))

  def sumList(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, Nil) => Nil
    case (Cons(a, t), Nil) => Cons(a, sumList(t, Nil))
    case (Nil, Cons(a, t)) => Cons(a, sumList(t, Nil))
    case (Cons(a1, t1), Cons(a2, t2)) => Cons(a1 + a2, sumList(t1, t2))
  }

  def addParWise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addParWise(t1, t2))
  }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def loop(list: List[A], sublist: List[A]): Boolean = (list, sublist) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (Cons(hl, tl), Cons(hs, ts)) => if (hl == hs) loop(tl, sub) else loop(tl, sublist)
    }

    loop(sup, sub)
  }



}

object Main extends App {

  import List._

  val x0 = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case _ => 101
  }

  println("x0:" + x0)

  val xs0: List[Int] = List(1, 2, 3, 4, 5)
  val ex10 = dropWhile(xs0, (x: Int) => x < 4)

  println("dropWhile: " + ex10)
  println("flatMap: " + flatMap(List(1, 2, 3))(i => List(i, i)))
  println("foldRight: " + foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))
  println("reverse: " + reverse(List(1, 2, 3)))
  println("appendLeft: " + appendLeft(List(1, 2, 3), List(4, 5, 6)))
  println("appendRight: " + appendRight(List(1, 2, 3), List(4, 5, 6)))
  println("concatenate: " + concatenate(List(List(1, 2, 3), List(4, 5, 6))))
  println("sublist: " + hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
  println("sublist: " + hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
  println("sublist: " + hasSubsequence(List(1, 2, 3, 4), List(4)))
  println("sublist: " + hasSubsequence(List(1, 2, 3, 4), List(1)))
  println("sublist: " + hasSubsequence(List(1, 2, 3, 4), Nil))
  println("sublist: " + hasSubsequence(List(1, 2, 3, 4), List(2, 4)))
}
