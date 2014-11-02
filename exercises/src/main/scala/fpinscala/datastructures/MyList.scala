package fpinscala.datastructures.ali

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => {
      if (n == 0) l
      else drop(xs, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => f(x) match {
      case true => dropWhile(xs, f)
      case false => l
    }
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(x, xs) => Cons(x, append(xs, l2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => {
      if (f(x)) xs
      else l
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(l: List[Int]) = foldRight(l, 0)((x, y) => x + y)

  def product2(l: List[Double]): Double = l match {
    case Nil => 0.0
    case Cons(0.0, _) => 0.0
    case _ => foldRight(l, 1.0)((x, y) => x * y)
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((x, y) => 1 + y)

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sumLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def prodLeft(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def lengthLeft[A](l: List[A]): Int = foldLeft(l, 0)((x, y) => x + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((x, y) => Cons(y, x))

  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(as, z)((x, y) => f(y, x))

  def sumLeft2(l: List[Int]): Int = foldLeft2(l, 0)((x, y) => x + y)

  def foldRight2[A, B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(l, z)((x, y) => f(y, x))

  def appendRight[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(x, xs) => foldRight(l1, l2)((x, y) => Cons(x, y))
  }

  def appendLeft[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(x, xs) => foldLeft(reverse(l1), l2)((y, x) => Cons(x, y))
  }

  def incrementList(ints: List[Int]): List[Int] = ints match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, incrementList(xs))
  }

  def incrementListLeft(ints: List[Int]): List[Int] =
    foldLeft(reverse(ints), Nil: List[Int])((y, x) => Cons(x + 1, y))

  def toStringForDoubleList(ds: List[Double]): List[String] =
    foldLeft(reverse(ds), Nil: List[String])((y, x) => Cons(x.toString, y))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldLeft(reverse(as), Nil: List[B])((y, x) => Cons(f(x), y))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldLeft(reverse(as), Nil: List[A])((y, x) => {
      if (f(x)) Cons(x, y)
      else y
    })

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldLeft(reverse(as), Nil: List[B])((y, x) => List.appendLeft(f(x), y))

  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => {
      if (f(x)) List(x)
      else Nil
    })

  def zipWithInt(l1: List[Int], l2: List[Int]): List[Int] = l1 match {
    case Nil => l2
    case Cons(x, xs) => l2 match {
      case Nil => l1
      case Cons(y, ys) => Cons(x + y, zipWithInt(xs, ys))
    }
  }

  def zipWith[A](l1: List[A], l2: List[A])(f: (A,A) => A): List[A] = l1 match {
    case Nil => l2
    case Cons(x, xs) => l2 match {
      case Nil => l1
      case Cons(y, ys) => Cons(f(x, y), zipWith(xs, ys)(f))
    }
  }

  def zipWithTailRec[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = {
    @tailrec
    def go(ll1: List[A], ll2: List[A], curr: List[A])(f: (A,A) => A): List[A] = ll1 match {
      case Nil => append(reverse(curr), ll2)
      case Cons(x, xs) => ll2 match {
        case Nil => append(reverse(curr), ll1)
        case Cons(y, ys) => go(xs, ys, Cons(f(x,y), curr))(f)
      }
    }

    go(l1, l2, Nil)(f)
  }

}

object Runner {
  def main(args: Array[String]) = {
    val l1 = List(1, 2, 3, 4, 5, 6, 7, 8)
    val l2 = List(1.0, 2.0, 3.0, 4.0, 5.0)
    val l3 = List(2, 3, 4, 5, 6, 7, 8, 9, 10)

    //    println(List.drop(List(1,2,3,4,5,6,7,8), 3))
    //    val f: Int => Boolean = (a: Int) => a < 4
    //    println(List.dropWhile(List(1,2,3,4,5,6,7,8), f))

    //    println(List.init(l1))

    //    println(List.dropWhile2(l1)(x => x < 4))

    //    println(List.foldRight(List(1,2,3), Nil: List[Int])(Cons(_,_)))

    //    println(List.sumLeft(l1))
    //    println(List.sum(l1))
    //    println(List.sum2(l1))
    //    println(List.sumLeft2(l1))

    //    println(List.prodLeft(l2))
    //    println(List.product(l2))
    //    println(List.product2(l2))

    //    println(List.lengthLeft(l1))
    //    println(List.length(l1))

    //    println(List.reverse(l1))

    //    val l3 = List(9, 10, 11)
    //
    //    println(List.append(l1, l3))
    //    println(List.appendRight(l1, l3))
    //    println(List.appendLeft(l1, l3))

    //    println(List.incrementList(l1))
    //    println(List.incrementListLeft(l1))

    //    println(List.toStringForDoubleList(l2))
    //    println(List.map(l2)(x => x.toString))

    //    println(List.filter(l1)(x => x%2 == 0))
    //    println(List.filterWithFlatMap(l1)(x => x%2 == 0))

    //    println(List.flatMap(l1)(x => List(x,x)))

    println(List.zipWithInt(l1,l3))
    println(List.zipWithInt(l3,l1))
    println(List.zipWith(l1,l3)((x,y)=>x+y))
    println(List.zipWith(l3,l1)((x,y)=>x+y))
    println(List.zipWithTailRec(l1,l3)((x,y)=>x+y))
    println(List.zipWithTailRec(l3,l1)((x,y)=>x+y))
  }
}
