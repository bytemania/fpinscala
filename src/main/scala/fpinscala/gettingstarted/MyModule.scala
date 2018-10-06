package fpinscala.gettingstarted

import scala.annotation.tailrec

object MyModule {

  def abs(n: Int): Int = if (n < 0) -n else n

//  private def formatAbs(x: Int) = {
//    val msg = "The absolute value of %d is %d."
//    msg.format(x, abs(x))
//  }

//  private def formatFactorial(n: Int) = {
//    val msg = "The factorial of %d is %d"
//    msg.format(n, factorial(n))
//  }

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = if (n <= 0) acc else go(n-1, n* acc)
    go(n, 1)
  }

  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def fib(n: Int): Int = {
    @tailrec
    def loop(n: Int, prev: Int, cur: Int): Int = {
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)
    }
    loop(n, 0 , 1)
  }

  def findFirst2(ss: Array[String], key: String): Int = {
    def loop(n: Int): Int =
      if (n >= ss.length) - 1
      else if (ss(n) == key) n
      else loop(n + 1)

    loop(0)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @tailrec
    def loop(i: Int): Int =
      if (i >= as.length) -1
      else if (p(as(i))) i
      else loop(i + 1)

    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(prev: Int, curr: Int): Boolean =
      if(curr == as.length) true
      else if(!ordered(as(prev), as(curr))) false
      else loop(curr, curr + 1)

    loop(0, 0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => B => C = (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))



  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println(formatResult("fibs: ", 10, n => (0 to n).map(fib).last))
    println("isSorted: " + isSorted((1 to 10).toArray, (x: Int, y: Int) => x <= y))
    println("findFirst: " + findFirst(Array(7, 9, 13), (x: Int) => x == 9))

    val f = (x: Double) => math.Pi / 2 - x

    val cos = compose(math.sin, f)

    println("cos(90) = " + cos(90))

  }
}
