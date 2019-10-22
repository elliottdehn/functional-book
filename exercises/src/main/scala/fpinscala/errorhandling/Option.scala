package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this.map(f).getOrElse(None)
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = {
    this.flatMap(_ => Some(this)).getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = {
    this.map(f) match {
      case Some(true) => this
      case Some(false) => None
      case None => None
    }
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(
      avg => mean(xs.map(elm => math.pow(elm-avg, 2)))
    )
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (None, _) => None
    case (_, None) => None
    case (Some(av), Some(bv)) => Some(f(av, bv))
  }

  //FAIL! But I was on the right track - I didn't know how to splice two lists together
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case List(h: Some[A], t) => Some(List())
    case List(None, t) => None
    case List() => None
  }

  //this was the correct answer
  def sequence_2[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }

  //Answer compare: I knew you could do this with map2 but went a different route...
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case h :: t => sequence(List(f(h))) flatMap(hl => traverse(t)(f).map(tl => hl :: tl))
    case Nil => Some(Nil)
    //inefficient
    //sequence(a.map(a => f(a)))
  }

  //correct answer:
  def traverse_2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h::t => map2(f(h), traverse(t)(f))(_ :: _)
    }

  //Answer compare: logically equivalent I think
  def sequence_3[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(elm => Some(elm.getOrElse(Nil)))
  }
}