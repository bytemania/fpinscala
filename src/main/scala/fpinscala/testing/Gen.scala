package fpinscala.testing

import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.state.{RNG, State}
import fpinscala.testing.Prop._
import fpinscala.laziness.Stream
import fpinscala.parallelism.Par
import fpinscala.parallelism.Par.Par

import scala.language.postfixOps

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Passed => p.run(max, n, rng)
      case x => x
    }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
      case x => x
    }
  }

  private def tag(msg: String) = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }

  case object Proved extends Result {
    override def isFalsified: Boolean = false
  }

  def check(p: => Boolean): Prop = Prop {(_, _, _) => if (p) Passed else Falsified("", 0)}

  val ES: ExecutorService = Executors.newCachedThreadPool
  val p1: Prop = Prop.forAll(Gen.unit(Par.unit(1)))(i =>
    Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)

  val p2: Prop = check {
    val p = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    p(ES).get == p2(ES).get
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p,p2)(_ == _)

  val p3: Prop = check {
    equal (
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    ) (ES) get
  }

  import Gen._

  val S: Gen[ExecutorService] = weighted(
    choose(1,4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25)

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop = props.map(p => Prop { (max, _, rng) => p.run(max, casesPerSize, rng) }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop = forAll(S.map2(g)((_, _))){case (s, a) => f(a)(s).get}

  def forAllPar2[A](g: Gen[A])(f: A => Par[Boolean]): Prop = forAll(S ** g){case (s, a) => f(a)(s).get}

  def forAllPar3[A](g: Gen[A])(f: A => Par[Boolean]): Prop = forAll(S ** g){case s ** a => f(a)(s).get}


  private def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  private def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop,
          maxSize: MaxSize = 100,
         testCases: TestCases = 100,
         rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Proved => println(s"+ OK, proved property.")
    }
  }

  def checkPar(p: Par[Boolean]): Prop = forAllPar(Gen.unit(()))(_ => p)

  val pint: Gen[Par[TestCases]] = Gen.choose(0, 10) map (Par.unit(_))

  val p4: Prop = forAllPar(pint)(n => equal(Par.map(n)(y => y), n))

  val pint2: Gen[Par[Int]] = choose(-100, 100).listOfN(choose(0, 20))
    .map(l => l.foldLeft(Par.unit(0))((p, i) => Par.fork {Par.map2(p, Par.unit(i))(_ + _)}))

  val forkProp: Prop = forAllPar(pint2)(i => equal(Par.fork(i), i)) tag "fork"

  val isEven: TestCases => Boolean = (i: Int) => i % 2 == 0

  def genStringIntFn(g: Gen[Int]): Gen[String => Int] = g map (i => s => i)
}

object ** {
  def unapply[A,B](p: (A,B)) = Some(p)
}

case class Gen[+A](sample: State[RNG, A]) {

  def listOfN(n: Int): Gen[List[A]] = Gen.listOfN(n, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] = size flatMap {s => this.listOfN(s)}

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = Gen(sample.map2(g.sample)(f))

  def unsized: SGen[A] = SGen(_ => this)

  def **[B](b: Gen[B]): Gen[(A, B)] = this.map2(b)((_, _))
}

object Gen {

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean flatMap (if (_) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n max 1))

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n))
}

case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] =  SGen(g(_).map(f))

  def map2[B, C](sg: SGen[B])(f: (A, B) => C): SGen[C] = SGen(n => g(n).map2(sg(n))(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(n => g(n).flatMap(f(_)(n)))

  def **[B](sg: SGen[B]): SGen[(A, B)] = SGen(n => apply(n) ** sg(n))

  def listOf[B >: A](g: Gen[B]): SGen[List[B]] = SGen(n => g.listOfN(n))
}

object GenApp extends App {

  val smallInt: Gen[TestCases] = Gen.choose(-10,10)

  val maxProp1 = forAll(Gen.listOf1(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max) // No value greater than `max` should exist in `l`
  }

  val sortedProp = forAll(Gen.listOf(smallInt)) {ns =>
    val nss = ns.sorted
    (nss.isEmpty || nss.tail.isEmpty || !nss.zip(nss.tail).exists {case (a, b) => a > b}) &&
    ns.forall(nss.contains) &&
    nss.forall(ns.contains)
  }

}

