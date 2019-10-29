package fpinscala.testing

import fpinscala.state._

import fpinscala.state.RNG.Simple

import language.postfixOps
import language.implicitConversions

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  def check: Boolean
  /*
  def &&(p: Prop): Boolean = {
   check && p.check
  }
  */

  /* We can refer to the enclosing `Prop` instance with `Prop.this` */
  def &&(p: Prop): Prop = new Prop {
    def check: Boolean = Prop.this.check && p.check
  }
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
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

