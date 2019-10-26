package fpinscala.state

import fpinscala.state.RNG.flatMap


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (i.abs + 1, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i.toDouble/Int.MaxValue.toDouble, r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val ip = rng.nextInt
    val dp = double(ip._2)
    val tuple = (ip._1, dp._1)
    (tuple, dp._2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val dp = double(rng)
    val ip = dp._2.nextInt
    val tuple = (dp._1, ip._1)
    (tuple, ip._2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val dp1 = double(rng)
    val dp2 = double(dp1._2)
    val dp3 = double(dp2._2)
    val triple = (dp1._1, dp2._1, dp3._1)
    (triple, dp3._2)
  }

  /*
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case 0 => (List[c Int](), rng)
    case a => (rng.nextInt :: ints(count - 1)(rng)._1, rng)
  }*/

  // A tail-recursive solution
  /*
  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
      if (count <= 0)
        (xs, r)
      else {
        val (x, r2) = r.nextInt
        go(count - 1, r2, x :: xs)
      }
    go(count, rng, List())
  } */

  def double2(rng: RNG): Rand[Double] = {
    map(nonNegativeInt)(i => i/Int.MaxValue.toDouble + 1)
  }

  /*
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    r: RNG => (f(ra, rb), r)
  }
  */

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  /*
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
  }*/

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng: RNG => {
      lazy val res = f(rng)
      g(res._1)(res._2)
    }
  }

  def map_f[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }

  /*
  def map2_f[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(rng => {
      val (r1, g) = ra(rng)
      val (r2, g2) = rb(g)
      ((r1, r2), g2)})((a, b: Ra) => )
}*/

  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit_s(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  object State {
    type Rand[A] = State[RNG, A]
    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
      State(m => )
    }
    def unit_s[S, A](a: A): State[S, A] =
      State(s => (a, s))

    // The idiomatic solution is expressed via foldRight
    def sequenceViaFoldRight[S,A](sas: List[State[S, A]]): State[S, List[A]] =
      sas.foldRight(unit_s[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get // Gets the current state and assigns it to `s`.
      _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
    } yield ()

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  }
}

