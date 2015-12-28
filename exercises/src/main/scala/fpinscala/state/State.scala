package fpinscala.state


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

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nextInt, nextRNG) = rng.nextInt
    if (nextInt > Int.MinValue) {
      (math.abs(nextInt), nextRNG)
    } else {
      nonNegativeInt(nextRNG)
    }
  }

  // Generate Double number between 0 (inclusive) and 1 (exclusive)
  def double(rng: RNG): (Double, RNG) = {
    val (nextInt, nextRNG) = nonNegativeInt(rng)
    val filterMaxInt = if (nextInt == Int.MaxValue) Int.MaxValue - 1 else nextInt
    val result: Double = filterMaxInt / Int.MaxValue
    (result, nextRNG)
  }

  def doubleWithMap(rng: RNG): (Double, RNG) =
    map(_.nextInt)(_.toDouble) apply rng

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (nextInt, rng1) = rng.nextInt
    val (nextDouble, rng2) = double(rng1)
    ((nextInt, nextDouble), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (nextDouble, rng1) = double(rng)
    val (nextInt, rng2) = rng1.nextInt
    ((nextDouble, nextInt), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (double1, rng1) = double(rng)
    val (double2, rng2) = double(rng1)
    val (double3, rng3) = double(rng2)
    ((double1, double2, double3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def loop(counter: Int, accum: List[Int], rng: RNG): (List[Int], RNG) = {
      if (counter > 0) {
        val (nextInt, nextRNG) = rng.nextInt
        loop(counter - 1, nextInt :: accum, nextRNG)
      } else {
        (accum.reverse, rng)
      }
    }

    loop(count, List(), rng)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (nextA, rngA) = ra apply rng
      val (nextB, rngB) = rb apply rngA

      (f(nextA, nextB), rngB)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      @annotation.tailrec
      def looper(ras: List[Rand[A]], curr: (List[A], RNG)): (List[A], RNG) = ras match {
        case List() => (curr._1.reverse, curr._2)
        case h :: t =>
          val (nextA, nextRNG) = h(curr._2)
          looper(t, (nextA :: curr._1, nextRNG))
      }
      looper(fs, (List(), rng))
    }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, nextRNG) = f(rng)
      g(a)(nextRNG)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(
      randInt => {
        nextRNG => {
          @annotation.tailrec
          def attempt(current: (Int, RNG)): (Int, RNG) = {
            println(s"current: ${current._1}, n: $n")
            if (current._1 < n) current
            else attempt(nonNegativeInt(current._2))
          }
          println(s"randInt: $randInt")
          attempt(randInt, nextRNG)
        }
      })

  def mapUsingFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => rng => (f(a), rng))

  def map2UsingFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    val bFlat: A => Rand[C] = a => flatMap(rb)(b => rng => (f(a, b), rng))
    flatMap(ra)(bFlat)
  }
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State[S, B] {
    s => {
      val (a, s1) = run(s)
      (f(a), s1)
    }
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State[S, C] {
    s => {
      val (a, s1) = run(s)
      val (b, s2) = sb.run(s1)
      (f(a, b), s2)
    }
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State[S, B] {
    s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    }
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object RNGState {
  type Rand[A] = State[RNG, A]

  val int: Rand[Int] = State[RNG, Int](_.nextInt)

  def ints(x: Int): Rand[List[Int]] =
    State[RNG, List[Int]](rng => {
      def looper(is: List[Int], r: RNG): (List[Int], RNG) = {
        if (is.size < x) {
          val (nextInt, nextRNG) = r.nextInt
          looper(nextInt :: is, nextRNG)
        } else {
          (is.reverse, r)
        }
      }
      looper(List(), rng)
    })

    val ns: Rand[List[Int]] =
      for {
        x <- int
        y <- int
        xs <- ints(x)
      } yield xs.map(_ % y)
}

object State {
  private def get[S]: State[S, S] = State(s => (s, s))

  private def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  def unit[S, A](a: A): State[S, A] = State[S, A](s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    @annotation.tailrec
    def loop(curr: State[S, List[A]], rest: List[State[S, A]]): State[S, List[A]] = rest match {
      case List() => curr
      case h :: t =>
        loop(curr.flatMap(as => {
          State[S, List[A]](s => {
            val (a, nextS) = h run s
            (a :: as, nextS)
          })
        }), t)
    }
    loop(unit(List()), fs)
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs map (modify[Machine] _ compose Machine.handle))
      m <- get
    } yield (m.candies, m.coins)
}

object Machine {
  def handle = (i: Input) =>
    (m: Machine) => (i, m) match {
      case (Coin, Machine(true, _, _)) if m.candies > 0 =>
        Machine(locked = false, m.candies, m.coins + 1)
      case (Turn, Machine(false, _, _)) if m.candies > 0 =>
        Machine(locked = true, m.candies - 1, m.coins)
      case _ => m
    }
}
