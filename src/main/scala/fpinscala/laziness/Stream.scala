package fpinscala.laziness

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

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

}
