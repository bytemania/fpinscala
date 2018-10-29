package fpinscala.parallelism

import java.util.concurrent.{Future, TimeUnit}

import scala.concurrent.duration.TimeUnit

object Map2Future {

  import Par._

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    val (af, bf) = (a(es), b(es))
    Map2Future(af, bf, f)
  }

}

case class Map2Future[A,B,C](a: Future[A], b: Future[B],
                             f: (A,B) => C) extends Future[C] {
  @volatile var cache: Option[C] = None
  def isDone: Boolean = cache.isDefined
  def isCancelled: Boolean = a.isCancelled || b.isCancelled
  def cancel(evenIfRunning: Boolean): Boolean =
    a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
  def get: C = compute(Long.MaxValue)
  def get(timeout: Long, units: TimeUnit): C =
    compute(TimeUnit.NANOSECONDS.convert(timeout, units))

  private def compute(timeoutInNanos: Long): C = cache match {
    case Some(c) => c
    case None =>
      val start = System.nanoTime
      val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
      val stop = System.nanoTime;val aTime = stop-start
      val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
      val ret = f(ar, br)
      cache = Some(ret)
      ret
  }
}
