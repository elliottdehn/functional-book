package fpinscala.laziness

import Stream.{unfold, _}
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = {
    this.foldRight(List())((h, acc) => h :: acc)
  }

  //incorrect
  def take_incorrect(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n-1))
    case _ => this
  }

  //correct take
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(_, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if p(h) => Cons(h, () => t().takeWhile(p))
    case _ => this
  }

  def forAll(p: A => Boolean): Boolean = {
    this.foldRight(false)((h, t) => p(h) || t)
  }

  def takeWhile_2(p: A => Boolean): Stream[A] = {
    this.foldRight(Empty[A])((h, t) => if(p(h)) cons(h,t) else Empty)
  }

  def headOption: Option[A] = {
    this.foldRight(None[A])((h, _) => Some(h))
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: => A => B): Stream[B] = {
    this.foldRight(Empty[B])((h, t) => cons(f(h), t))
  }

  def filter(f: A => Boolean): Stream[A] = {
    this.foldRight(Empty[A])((h, t) => if(f(h)) cons(h, t) else t)
  }

  def append[B>:A](l: => Stream[B]): Stream[B] = {
    this.foldRight(l)((h, t) => cons(h, t))
  }

  def flatmap[B](f: A => Option[B]): Stream[Option[B]] = {
    this.map(f).foldRight(Empty[A])((h, t) => cons(f(h), t))
  }

  def map2[B](f: => A => B): Stream[B] = {
    Stream.unfold(this)({case Stream(h, t: Stream[A]) => Some((f(h), t))})
  }

  //Shame: I gave up on this problem and looked at the answer - turns out I was missing a key concept.

  
  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    Stream.cons(a, constant(a))
  }
  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n+1))
  }

  def fibs(): Stream[Int] = {
    def fibsFrom(n1: Int, n2: Int): Stream[Int] = {
      Stream.cons(n2, fibsFrom(n2, n1 + n2))
    }

    Stream.cons(0, fibsFrom(0, 1))
  }

  //incorrect
  def unfold_inc[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    Stream.cons(f(z).get._1, unfold(f(z).get._2)(f))
  }

  //correct
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }

  def fibs2(): Stream[Int] = {
    unfold((0,1))(s => Some(s._1 + s._2, (s._2, s._1 + s._2)))
  }

  def from2(n: Int): Stream[Int] = {
    unfold(n)(v => Some(v + 1, v + 1))
  }

  def constant2(n: Int): Stream[Int] = {
    unfold(n)(v => Some(v, v))
  }

  def ones2(): Stream[Int] = {
    constant2(1)
  }


}