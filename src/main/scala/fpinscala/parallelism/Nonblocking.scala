package fpinscala.parallelism

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference

object Nonblocking {

  sealed trait Future[A] {
    private[parallelism] def apply(k: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  object Par {

    def run[A](es: ExecutorService)(p: Par[A]): A = {
      def ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)
      p(es){a =>
        ref.set(a)
        latch.countDown()
      }
      latch.await()
      ref.get
    }

    def unit[A](a: A): Par[A] = es => new Future[A] {
      override private[parallelism] def apply(cb: A => Unit): Unit = cb(a)
    }

    def delay[A](a: => A): Par[A] = es => new Future[A] {
      override private[parallelism] def apply(cb: A => Unit): Unit = cb(a)
    }

    def fork[A](a: => Par[A]): Par[A] = es => new Future[A] {
      override private[parallelism] def apply(cb: A => Unit): Unit = eval(es)(a(es)(cb))
    }

    def eval(es: ExecutorService)(r: => Unit): Unit = es.submit(new Callable[Unit] {def call: Unit = r})

    def async[A](f: (A => Unit) => Unit): Par[A] = es => new Future[A] {
      def apply(k: A => Unit) = f(k)
    }

    def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = es => new Future[C] {
      def apply(cb: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None

        val combiner = Actor[Either[A, B]](es) {
          case Left(a) => br match {
            case None => ar = Some(a)
            case Some(b) => eval(es)(cb(f(a, b)))
          }

          case Right(b) => ar match {
            case None => br = Some(b)
            case Some(a) => eval(es)(cb(f(a, b)))
          }
        }

        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }
    }

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = es => new Future[A] {
      override private[parallelism] def apply(cb: A => Unit): Unit = cond(es) { b =>
        if (b) eval(es) {t(es)(cb)}
        else eval(es) {f(es)(cb)}
    }}

    def choiceN[A](p: Par[Int])(ps: List[Par[A]]): Par[A] = es => new Future[A] {
      override private[parallelism] def apply(cb: A => Unit): Unit = p(es) {ind => eval(es) {ps(ind)(es)(cb)}}
    }

    def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

    def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
      choiceN(map(a)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))

    def choiceMap[K, V](p: Par[K])(ps: Map[K, Par[V]]): Par[V] = es => new Future[V] {
      override private[parallelism] def apply(cb: V => Unit): Unit = p(es)(k => ps(k)(es)(cb))
    }

    def flatMap[A, B](p: Par[A])(f: A => Par[B]): Par[B] = es => new Future[B] {
      override private[parallelism] def apply(cb: B => Unit): Unit = p(es)(a => f(a)(es)(cb))
    }

    def chooser[A, B](p: Par[A])(f: A => Par[B]): Par[B] = flatMap(p)(f)

    def choiceViaFlatMap[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = flatMap(p)(b => if (b) t else f)

    def choiceNViaFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] = flatMap(p)(choices(_))

    def join[A](p: Par[Par[A]]): Par[A] = es => new Future[A] {
      override private[parallelism] def apply(cb: A => Unit): Unit = p(es)(p2 => eval(es) {p2(es)(cb)})
    }

    def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] = flatMap(a)(x => x)

    def flatMapViaJoin[A, B](p: Par[A])(f: A => Par[B]): Par[B] = join(map(p)(f))

    implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

    class ParOps[A](p: Par[A]) {
      def map[B](f: A => B): Par[B] = Par.map(p)(f)
      def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] = Par.map2(p, b)(f)
      def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(p)(f)
      def zip[B](b: Par[B]): Par[(A, B)] = p.map2(b)((_, _))
    }

  }
}
