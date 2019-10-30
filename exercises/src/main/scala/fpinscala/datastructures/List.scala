package fpinscala.datastructures

import fpinscala.datastructures.List.concat

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  //answer compare: did not throw an exception because I thought those were avoided in FP :-)
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  //answer compare: dead on!
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("No head to replace")
    case Cons(_,t) => Cons(h,t)
  }

  //Answer compare: I threw an error on trying to drop from an empty list
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => sys.error("Nothing to drop")
    case Cons(_,t) => drop(t,n-1)
  }


  //Answer comparison: significant diversion, if not incorrect. I threw an error. Also used a previous fn
  //Pattern guard, a feature I wasn't aware of, does indeed make this easier!
  @scala.annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    def getHead: A = l match {
      case Nil => sys.error("No head")
      case Cons(h,_) => h
    }

    if(f(getHead)){
      dropWhile(tail(l), f)
    } else {
      l
    }
  }

  //Answer comparison: Close! I did not throw an exception on initializing Nil
  def init[A](l: List[A]): List[A] =
  l match {
    case Cons(_, Nil) => Nil
    case Cons(h,t) => Cons(h, init(t))
  }

  //Answer comparison: Dead on!
  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, y) => 1 + y)
  }

  //Answer comparison: I incorrectly had a case for Cons(h, Nil)
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h1, t) => foldLeft(t, f(z, h1))(f)
  }

  //Answer compares to all 3: I had a case for Nil, but didn't need it
  def sum3(ints: List[Int]): Int = ints match {
    case Nil => 0
    case _ => foldLeft(ints, 0)(_ + _)
  }

  //Nil should have returned 1.0, anyway
  def product3(ds: List[Double]): Double = ds match {
    case Nil => 0
    case _ => foldLeft(ds, 1.0)(_ * _)
  }

  def length2[A](l: List[A]): Int = l match {
    case Nil => 0
    case _ => foldLeft(l, 0)((x, _) => x + 1)
  }

  /*
  //Answer compare: close. I used a Nil case when I could have used a one-liner with List[A]() for z
  def reverse[A](l: List[A]): List[A] = {
    case Nil => Nil
    case Cons(h, _) => foldLeft(l, Cons(h, Nil))((x,y) => Cons(y, x))
  }*/

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc,h) => Cons(h,acc))

  def append_fl[A](l: List[A], app: List[A]): List[A] = {
    foldLeft(reverse(l), app)((acc: List[A], x: A) => Cons(x, acc))
  }

  //Compared to answer: correct. I should have simply used f = Cons(_,_)
  def append_fr[A](l: List[A], app: List[A]): List[A] = {
    foldRight(l, app)((x, acc) => Cons(x, acc))
  }

  //not tail recursive, but correct?
  def flatten[A](l: List[List[A]]): List[A] = l match {
    case Nil => Nil
    case Cons(fl, t) => append(fl, flatten(t))
  }

  //Answer comparison: I did not use fold right, but I think this is correct
  //It is functionally in the same vein: concatenate the first list with the second
  //and do this until you run out of lists. Return the flattened flat list.
  def flatten_tr[A](l: List[List[A]]): List[A] = l match {
    case Nil => Nil
    case Cons(fl, Nil) => fl
    case Cons(fl, Cons(sl, t)) => flatten_tr(Cons(append(fl, sl), t))
  }

  //Answer comparison: dead on!
  def inc_one(l: List[Int]): List[Int] = {
    foldRight(l, Nil:List[Int])((ne, acc) => Cons(ne + 1, acc))
  }

  //Answer comparison: dead on!
  def d_ts(l: List[Double]): List[String] = {
    foldRight(l, Nil:List[String])((ne, acc) => Cons(ne.toString, acc))
  }

  //Answer comparison: I did the "natural" solution but the typical sol. is a buffer
  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil:List[B])((ne, acc) => Cons(f(ne), acc))
  }

  //Answer comparison: I knew the "real" solution was a buffer, but my fold right solution is correct
  def filter[A](l: List[A], f: A => Boolean): List[A] = {
    foldRight(l, Nil:List[A])((ne, acc) => if(f(ne)) Cons(ne, acc) else acc)
  }

  /*
  //Answer comparison: could have used map and made this a lot simpler
  def flatMap[A, B](l: List[A], f: A => List[A]): List[A] = {
    val buf = new collection.mutable.ListBuffer[List[A]]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h,t) => buf += f(h); go(t)
    }
    go(l)
    flatten(List(buf.toList: _*))
  }*/

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  /*
  This could also be implemented directly using `foldRight`.
  */
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  /*
  //Answer comparison: dead on!
  def filter_fm[A](l: List[A], f: A => Boolean): List[A] = {
    flatMap(l, x => if (f(x)) Cons(x, Nil) else Nil:List[A])
  }*/

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  //Answer compare: close, just didn't have cases for _,Nil and Nil,_
  def addLists(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addLists(t1, t2))
  }

  //Answer compare: nailed it!
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil,_) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }

  //Answer compare: I got pretty close! I did it in one method and I'm not 100% sure mine is actually correct
  //but it is in the same vein: seek the start of the subsequence, then check if you find it. Else keep searching.
  //Return false if you reach the end of the list.
  @scala.annotation.tailrec
  def subsequence[A](l: List[A], sq: List[A]): Boolean = (l, sq) match {
    case (Nil, Cons(_,_)) => false
    case (_, Nil) => true
    case (Cons(h, t), Cons(hsq, tsq)) if h == hsq => subsequence(t,tsq)
    case (Cons(_, t), Cons(_, _)) => subsequence(t, sq)
  }

}
