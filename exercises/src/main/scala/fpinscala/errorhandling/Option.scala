package fpinscala.errorhandling

import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

//  def flatMap[B](f: A => Option[B]): Option[B] = this match {
//    case None => None
//    case Some(a) => f(a)
//  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => {
      if (f(a)) Some(a)
      else None
    })
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  //s = Seq(a), m = mean(s) => variance = mean({math.pow(x - m, 2) | each x in s})
  def variance(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(x => b.map(y => f(x,y)))
  }

  def sequence[A](options: List[Option[A]]): Option[List[A]] =
    options.reverse.foldLeft(Some(Nil): Option[List[A]])((optionOfList, optionOfA) =>
      optionOfA.flatMap(a => optionOfList.flatMap(as => Some(a :: as))))

  def traverse[A,B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.reverse.foldLeft(Some(Nil): Option[List[B]])((optionOfList: Option[List[B]], a: A) =>
      f(a).flatMap(b => optionOfList.flatMap(bs => Some(b :: bs))))

  def sequence2[A](options: List[Option[A]]): Option[List[A]] =
    traverse(options)(a => a)
}

object OptionRunner {
  def main(args: Array[String]): Unit = {
    val options: List[Option[Int]] = List(Some(1), Some(2), Some(3), Some(4), Some(5))
//    System.out.println(Option.sequence(options))
//    System.out.println(Option.sequence2(options))

//    val numbers = List(1,2,3,4,5,6)
//    System.out.println(Option.traverse(numbers)(i => Some("number " + i)))
  }
}
