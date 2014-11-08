package fpinscala.datastructures.ali

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  //Without folds:

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

  //Fold Right:

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

  def append2[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(x, xs) => foldRight(l1, l2)((x, y) => Cons(x, y))
  }

  //Fold Left:

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def prod2(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((x, y) => x + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((x, y) => Cons(y, x))

  //Fold left using fold right(useless)
//  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(l), z)((x, y) => f(y, x))

//  def sum4(l: List[Int]): Int = foldLeft2(l, 0)((x, y) => x + y)

  //Fold right using fold left
  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)((x, y) => f(y, x))

  def append3[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(x, xs) => foldRight2(l1, l2)((x, y) => Cons(x, y))
  }

  def append4[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(x, xs) => foldLeft(reverse(l1), l2)((y, x) => Cons(x, y))
  }

  //Non-stack-safe
  def appendAll[A](ls: List[List[A]]): List[A] = ls match {
    case Nil => Nil
    case Cons(l1, tail) => append4(l1, appendAll(tail))
  }

  //Stack-safe
  def appendAll2[A](ls: List[List[A]]): List[A] =
    foldLeft(reverse(ls), Nil: List[A])((l1, l2) => append4(l2, l1))

  def incrementList(ints: List[Int]): List[Int] = ints match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, incrementList(xs))
  }

  def incrementList2(ints: List[Int]): List[Int] =
    foldLeft(reverse(ints), Nil: List[Int])((y, x) => Cons(x + 1, y))

  def toStringForDouble(ds: List[Double]): List[String] =
    foldLeft(reverse(ds), Nil: List[String])((y, x) => Cons(x.toString, y))

  //Map:
  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldLeft(reverse(as), Nil: List[B])((y, x) => Cons(f(x), y))

  def toString2[A](as: List[A])(f: A => String): List[String] = map(as)(f)

  def toStringForDoubles2(ds: List[Double]): List[String] =
    toString2(ds)(d => d.toString)

  //Filter:
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldLeft(reverse(as), Nil: List[A])((y, x) => {
      if (f(x)) Cons(x, y)
      else y
    })

  //FlatMap:
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldLeft(reverse(as), Nil: List[B])((y, x) => append4(f(x), y))

  //Filter with FlatMap:
  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => {
      if (f(x)) List(x)
      else Nil
    })

  def addCorrespondingItems(l1: List[Int], l2: List[Int]): List[Int] = l1 match {
    case Nil => l2
    case Cons(x, xs) => l2 match {
      case Nil => l1
      case Cons(y, ys) => Cons(x + y, addCorrespondingItems(xs, ys))
    }
  }

  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = l1 match {
    case Nil => l2
    case Cons(x, xs) => l2 match {
      case Nil => l1
      case Cons(y, ys) => Cons(f(x, y), zipWith(xs, ys)(f))
    }
  }

  def zipWith2[A](l1: List[A], l2: List[A])(f: (A,A) => A): List[A] = {
    @tailrec
    def go(list1: List[A], list2: List[A], cur: List[A]): List[A] = (list1, list2) match {
      case (_, Nil) => append4(reverse(cur), list1)
      case (Nil, _) => append4(reverse(cur), list2)
      case (Cons(x, xs), Cons(y, ys)) => go(xs, ys, Cons(f(x, y), cur))
    }

    go(l1, l2, Nil: List[A]);
  }
}

object Runner {
  def main(args: Array[String]) = {
    val l1 = List(1, 2, 3, 4, 5, 6, 7, 8)
    val l2 = List(1.0, 2.0, 3.0, 4.0, 5.0)
    val l3 = List(2, 3, 4, 5, 6, 7, 8, 9, 10)
    val l4 = List(9, 10, 11)

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

//  println(List.append(l1, l3))
//  println(List.append2(l1, l3))
//  println(List.append3(l1, l3))
//  println(List.append4(l1, l3))

//    println(List.incrementList(l1))
//    println(List.incrementListLeft(l1))

//    println(List.toStringForDoubleList(l2))
//    println(List.map(l2)(x => x.toString))

//    println(List.filter(l1)(x => x%2 == 0))
//    println(List.filterWithFlatMap(l1)(x => x%2 == 0))

//    println(List.flatMap(l1)(x => List(x,x)))

//    println(List.zipWithInt(l1,l3))
//    println(List.zipWithInt(l3,l1))
//    println(List.zipWith(l1,l3)((x,y)=>x+y))
//    println(List.zipWith(l3,l1)((x,y)=>x+y))

//    println(List.zipWith2(l1, l3)((x, y) => x + y))
//    println(List.zipWith2(l3, l1)((x, y) => x + y))

//    println(List.appendAll2(List(l1,l2,l3,l4)))
  }
}
