package fpinscala.laziness

import scala.annotation.tailrec

sealed trait Stream[+A] {

  def headOptionPatternMatching: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def headOption: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))

  def toListRecursive: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toListRecursive
  }

  def toList: List[A] = {
    @tailrec
    def loop(acc: List[A], s: Stream[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => loop(acc :+ h(), t())
    }

    loop(Nil, this)
  }

  import Stream._

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n -1)
    case _ => this
  }

  def takeWhileRecursive(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhileRecursive(p))
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if(p(h)) cons(h, t) else empty)



  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case Empty => z
  }

  def existsUnsafe(p: A => Boolean): Boolean = foldRight(false)(p(_) || _)

  def forAll(p: A => Boolean): Boolean = foldRight(true)(p(_) && _)

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) =>  if(p(h)) cons(h, t) else t)

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, t) => f(h).append(t))

  def append[B >: A](that: => Stream[B]): Stream[B] = foldRight(that)((h, t) => cons(h, t))

  def find(p: A => Boolean): Option[A] = filter(p).headOption
}


case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constantRecursive[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def constant[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  def fromRecursive(n: Int): Stream[Int] = cons(n, fromRecursive(n + 1))

  def from(n: Int): Stream[Int] = unfold(n)(x => Some((x, x + 1)))

  val fibsRecursive: Stream[Int] = {
    def loop(n0: Int, n1: Int): Stream[Int] =
    {
      cons(n0, loop(n1, n0 + n1))
    }
    loop(0, 1)
  }

  val fibs: Stream[Int] = unfold((0, 1)){case (f0, f1) => Some((f0, (f1 , f0 + f1)))}

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty[A]
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  val onesRecursive: Stream[Int] = Stream.cons(1, onesRecursive)

  val ones: Stream[Int] = unfold(1)(_ => Some((1, 1)))

}


object MainStream extends App {
  def if2[A](cond: Boolean, onTrue: => A, onFalse:  => A): A = if(cond) onTrue else onFalse

  println("lazy if: ")
  val a = 10
  if2(a < 22, println("a"), println("b"))
  println("3: " + if2[Int](cond = false, sys.error("fail"), 3))

  def maybeTwice(b: Boolean, i: => Int): Int = {
    lazy val j = i
    if(b) j + j else 0
  }

  val x = maybeTwice(true, {println( "hi"); 1 + 41})
  println("x: " + x)

  println("Stream(1,2,3).toList: " + Stream(1, 2, 3).toList)

  import Stream._

  println("ones: " + ones.take(5).toList)
  println("ones.exists:" + ones.exists(_ % 2 != 0))
  println("ones.map: " + ones.map(_ + 1).exists(_ % 2 == 0))
  println("takeWhile: " + ones.takeWhile(_ == 1))
  println("ones:" + ones.forAll(_ != 1))

}
