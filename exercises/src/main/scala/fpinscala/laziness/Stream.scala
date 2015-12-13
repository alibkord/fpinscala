package fpinscala.laziness

import Stream._


sealed trait Stream[+A] {

  // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument
  // by name and may choose not to evaluate it.
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns
    // `true`, `b` will never be evaluated and the computation terminates early.
    foldRight(false)((a, b) => p(a) || b)

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = foldRight(List[A]())((h, la) => h :: la)

  def take(n: Int): Stream[A] = {
    @annotation.tailrec
    def loop(counter: Int, result: Stream[A], tail: Stream[A]): Stream[A] = tail match {
      case Cons(h, t) if counter < `n` =>
          loop(counter + 1, Cons(h, () => result), t())
        //TODO: reverse
      case _ => result
    }

    loop(0, Empty, this)
  }

  def drop(n: Int): Stream[A] = {
    @annotation.tailrec
    def loop(counter: Int, stream: Stream[A]): Stream[A] = stream match {
      case Cons(h, t) if counter < `n` =>
        println("counter=" + counter)
        loop(counter + 1, t())
      case _ => stream
    }

    loop(0, this)
  }

  def takeWhile(p: A => Boolean): Stream[A] = sys.error("todo")

  def forAll(p: A => Boolean): Boolean = sys.error("todo")

  def headOption: Option[A] = sys.error("todo")

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}

// Stream definition

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

// Stream singleton object:

object Stream {
  // "Smart constructor"
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  // empty stream
  def empty[A]: Stream[A] = Empty

  // factory method
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}

object Tester {
  def main(args: Array[String]) {
    val stream1 = Stream(1, 2, 3, 4)

    println("toList: " + stream1.toList)
    println("Take 2: " + stream1.take(2).toList)
    println("Drop 2: " + stream1.drop(2).toList)
  }
}
