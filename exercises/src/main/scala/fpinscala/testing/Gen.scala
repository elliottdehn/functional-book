package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state.{RNG, _}
import fpinscala.state.RNG.Simple
import fpinscala.testing.Prop.{FailedCase, Falsified, Passed, Result, SuccessCount, TestCases}

import language.postfixOps
import language.implicitConversions

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}
case class Prop(run: (TestCases,RNG) => Result)

object Prop {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }


  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = {
    Gen(sample.map(f))
  }

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(State(s => {
      val (a, rng) = sample.run(s)
      f(a).sample.run(rng)
    }))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(i => Gen(State.sequence(List.fill(i)(sample))))
  }

}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))

  def unit[A](a: => A): Gen[A] = {
    Gen(State.unit_s(a))
  }

  def genOption[A](g: Gen[A]): Gen[Option[A]] = {
    genMap(g)(a => Some(a))
  }

  def genFromOption[A](g: Gen[Option[A]]): Gen[A] = {
    genMap(g)(o => o.get)
  }

  def genMap[A,B](g: Gen[A])(f: A => B): Gen[B] = {
    Gen(g.sample.map(f))
  }

  def boolean: Gen[Boolean] = {
    Gen(State(RNG.boolean))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap(b => if (b) g1 else g2)
  }

  /*
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = (g1, g2) match {
    case ((g, d), (g2, _)) => Gen(State(RNG.double)) flatMap(dg => if (dg < d) g else g2)
  }*/ //my answer: incorrect

  /*
  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
  /* The probability we should pull from `g1`. */
  val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

  Gen(State(RNG.double).flatMap(d =>
    if (d < g1Threshold) g1._1.sample else g2._1.sample))
}
   */
}

trait SGen[+A] {

}

