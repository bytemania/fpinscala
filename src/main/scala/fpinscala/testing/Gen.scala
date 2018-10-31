package fpinscala.testing

import fpinscala.state.{RNG, State}
import fpinscala.testing.Prop.{FailedCase, SuccessCount}

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(p: Prop): Prop = new Prop {
    override def check: Unit = Prop.this && p
  }

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

case class Gen[A](sample: State[RNG, A]) {

  def listOfN(n: Int, g: Gen[A]): Gen[List[A]] = Gen.listOfN(n, g)

  def listOfN(n: Int): Gen[List[A]] = Gen.listOfN(n, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] = size flatMap {s => this.listOfN(s)}

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))
}

object Gen {

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean flatMap (if (_) g1 else g2)

  def weigthted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }
}
