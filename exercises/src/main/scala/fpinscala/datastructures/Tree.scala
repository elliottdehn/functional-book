package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  //Answer compare: I misread the problem and only counted leaves. Added the 1 + on the branch.
  def count_leaves[A](t: Tree[A]): Integer = t match {
    case Leaf(_) => 1
    case Branch(t1, t2) => 1 + count_leaves(t1) + count_leaves(t2)
  }

  //answer compare: mine is correct, but excessive. I didn't need the internal function.
  def tree_max(t: Tree[Int]): Int = {
    def go(t: Tree[Int], m: Int): Int = t match {
      case Leaf(v) => m.max(v)
      case Branch(l, r) => tree_max(l).max(tree_max(r))
    }

    go(t, 0)
  }

  //Answer compare: dead on!
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + depth(l).max(depth(r))
  }

  //answer compare: dead on!
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  //answer compare: surprisingly dead on, except for depth which i made a simple mistake on.
  //i fixed the depth to be correct.
  def fold[A, B](t: Tree[A])(lf: A => B)(bf: (B, B) => B): B = t match {
    case Leaf(v) => lf(v)
    case Branch(l, r) => bf(fold(l)(lf)(bf), fold(r)(lf)(bf))
  }

  def count_2[A](t: Tree[A]): Int = {
    fold(t)(_ => 1) ((l, r) => 1 + l + r)
  }

  def tree_max2(t: Tree[Int]): Int = {
    fold(t)(x => x)(_ max _)
  }

  def tree_depth2[A](t: Tree[A]): Int = {
    fold(t)(_ => 0)(_ max _)
  }

  //also had to fix some type annotations here
  def tree_map2[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(x => Leaf(f(x)): Tree[B])((l, r) => Branch(l, r))
  }
}