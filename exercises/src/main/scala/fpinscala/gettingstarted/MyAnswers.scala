package fpinscala.gettingstarted

object SortedNess {
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(as: Array[A], i: Int, ordered: (A,A) => Boolean): Boolean = {
      if (as.length <= 1 || i == as.length - 1) true
      else if (!ordered.apply(as(i), as(i+1))) false
      else loop(as, i +1, ordered)
    }

    loop(as, 0, ordered)
  }

  def main(args: Array[String]): Unit = {
    println(isSorted(Array[Int](3,2,3,5,5), (a1: Int, a2: Int) => a1.compareTo(a2) <= 0))
  }
}

object Curry {
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a,b)

  def main(args: Array[String]): Unit = {
    println(curry[Int,Int,Int]((a, b) => a+b)(10)(20))
  }
}

object Uncurry {
  def uncurry[A,B,C](f: A => B => C): (A,B) => C = (a,b) => f(a)(b)

  def main(args: Array[String]): Unit = {
    println(uncurry[Int,Int,Int](a => b => a+b)(20,30))
  }
}

object Partial {
  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = (b: B) => f(a,b)

  def main(args: Array[String]): Unit = {
    println(partial1[Int,Int,Int](10, (a: Int,b: Int) => a+b)(5))
  }
}

object Composition {
  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

  def main(args: Array[String]): Unit = {
    println(compose[Int,Int,Int](a => a*2, a => a*3)(3))
  }
}
