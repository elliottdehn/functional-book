package fpinscala.testing

import fpinscala.state
import fpinscala.state.RNG
import fpinscala.state.RNG.State

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

case class Gen[+A](sample: State[RNG, A])

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
  }
}

trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}

