package fpinscala.parallelism

import java.util.concurrent.ExecutorService

import java.util.concurrent._
import language.implicitConversions


object Par {
  type Par[A] = ExecutorService => Future[A]

 /* creates a computation that immediately results in the value a
  * unit is represented as a function that returns UnitFuture, which is a
  * simple implementation of Future that just wraps a constant value. It doesn't
  * use the ExecutorService at all. It's always done and can't be cancelled. Its
  * get method simply returns the value that we gave it.
  */
  def unit[A](a: => A): Par[A] = _ => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone: Boolean = true
    override def get(l: Long, timeUnit: TimeUnit): A = get
    override def isCancelled: Boolean = false
    override def cancel(b: Boolean): Boolean = false
  }

  /* combines the results of two parallel computations witha a binary function
   * map2 doesn't evaluate the call to f in a separate logical thread, in accord
   * with our design choice of having fork be the sole function in the API for
   * controlling parallellism. We can always do fork(map2(a,b)(f)) if we want
   * the evaluation of f to occur in a separated thread.
   */
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = es => {
    val pf: Future[C => D] = map2(a, b)(f.curried(_)(_))(es)
    val cf = c(es)
    val res = pf.get()(cf.get())
    UnitFuture(res)
  }

  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] = es => {
    val pf = map3(a, b, c)(f.curried(_)(_)(_))(es)
    val df = d(es)
    val res = pf.get()(df.get())
    UnitFuture(res)
  }

  def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => F): Par[F] =
    es => {
      val pf = map4(a, b, c, d)(f.curried(_)(_)(_)(_))(es)
      val ef = e(es)
      val res = pf.get()(ef.get())
      UnitFuture(res)
    }

  //marks a computation for concurrent evaluation by run
  def fork[A](fa: => Par[A]): Par[A] = es => es.submit(() => fa(es).get)

  //wraps the expression a for concurrent evaluation by run
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  //fully evaluates a given Par, spawing parallel computations as requested by
  //fork and extracting the resulting value
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = 
    ps.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))

  def parFilter[A](as: List[A])(f: A => Boolean) : Par[List[A]] = {
    val pars: List[Par[List[A]]] = as map asyncF(a => if(f(a)) List(a) else Nil)
    val parsList: Par[List[List[A]]] = sequence(pars)
    map(parsList)(_.flatten)
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = es => if (run(es)(cond).get) t(es) else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
    val ind = run(es)(n).get
    run(es)(choices(ind))
  }

  def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    choiceN(map(a)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = es => run(es)(choices(run(es)(key).get))

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = flatMap(pa)(choices)

  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = flatMap(cond)(b => if (b) t else f)

  def choiceNViaChoose[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = flatMap(n)(choices(_))

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = es => {
    val k = run(es)(a).get
    run(es)(f(k))
  }

  def join[A](a: Par[Par[A]]): Par[A] = flatMap(a)(a => a)

  def joinSimple[A](a: Par[Par[A]]): Par[A] = es => run(es)(run(es)(a).get())

  def flatMapViaJoin[A, B](p: Par[A])(f: A => Par[B]): Par[B] = join(map(p)(f))

  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {
    def map[B](f: A => B): Par[B] = Par.map(p)(f)
    def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] = Par.map2(p, b)(f)
    def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(p)(f)
    def zip[B](b: Par[B]): Par[(A, B)] = p.map2(b)((_, _))
  }

}

object Examples extends App {
  import Par._

  def sumSequential(ints: Seq[Int]): Int = ints.sum

  def sumSplit(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sumSplit(l) + sumSplit(r)
    }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

  def maximumParList(l: List[Int]): Par[Int] =
    l.foldRight[Par[Int]](unit(Int.MinValue))((h, t) => map2(unit(h), t)((a, b) => if (a > b) a else b))

  def wordsPar(l: List[String]): Par[Int] =
    l.foldRight[Par[Int]](unit(0))((h, t) => map2(unit(h), t)(_.split(" ").length + _))

  import java.util.concurrent.Executors

  val executor = Executors.newFixedThreadPool(10)

  val max: Int = run(executor)(maximumParList(List(1, 5, 6, 7, 0))).get()

  val wordsPars = run(executor)(wordsPar(List("one two three", "four five"))).get()

  println("maximum:" + max)
  println("words:" + wordsPars)

  val mapLaw1 = equal(executor)(map(unit(1))(_ + 1), unit(2))

  println("mapLaw1: " + mapLaw1)




}
