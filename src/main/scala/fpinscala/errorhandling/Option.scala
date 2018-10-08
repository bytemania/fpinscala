package fpinscala.errorhandling

sealed trait Option[+A]{
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(get) => Some(f(get))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(get) => get
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = Some(this).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = this.flatMap(x => if(f(x)) Some(x) else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _.map(f)

  def map2FlatMap[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(x => b.map(f(x, _)))

  def map3[A, B, C, D](a: Option[A], b: Option[B], c: Option[C])(f: (A, B, C) => D): Option[D] =
    a.flatMap(aa => b.flatMap(bb => c.map(f(aa, bb, _))))

  def map4[A, B, C, D, E](a: Option[A], b: Option[B], c: Option[C], d: Option[D])(f: (A, B, C, D) => E): Option[E] =
    a.flatMap(aa => b.flatMap(bb => c.flatMap(cc => d.map(f(aa, bb, cc, _)))))

  def map5[A, B, C, D, E ,F](a: Option[A], b: Option[B], c: Option[C], d: Option[D], e: Option[E])
                            (f: (A, B, C, D, E) => F): Option[F] =
    a.flatMap(aa => b.flatMap(bb => c.flatMap(cc => d.flatMap(dd => e.map(f(aa, bb, cc, dd, _))))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
    aa <- a
    bb <- b
  } yield f(aa, bb)

  def sequenceRecursive[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap(hh => sequenceRecursive(t) map (hh :: _))
  }

  def traverseSequence[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sequenceRecursive(a map f)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)

}

object MainOption extends App {
  def Try[A](a: => A): Option[A] = try Some(a) catch {case _: Exception => None}

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    Option.map2(optAge, optTickets)(insuranceRateQuote)
  }

  def parseInts(a: List[String]): Option[List[Int]] = Option.sequence(a map (i => Try(i.toInt)))

  println("ParseInts:" + List("1", "2", "3"))

}
