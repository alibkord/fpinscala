package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _}

// hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(error) => Left(error)
    case Right(value) => Right(f(value))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(error) => Left(error)
    case Right(value) => f(value)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case _ => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(a => b.map(bb => f(a, bb)))

  def map2_2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)

}

case class Left[+E](get: E) extends Either[E, Nothing]
case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.reverse.foldLeft(Right(Nil: List[B]): Either[E, List[B]])((elb, a) => elb.flatMap(lb => f(a).map(bb => bb :: lb)))

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(ea => ea.map(aa => aa))

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }
}

object EitherRunner {
  def main(args: Array[String]): Unit = {
    //    val e1: Either[String,Int] = Right(1)
    //    val e2: Either[String,Int] = Left("Not good!")
    //
    //    System.out.println(e1.map2(Right(2))(_ + _))
    //    System.out.println(e2.map2(Right(2))(_ + _))
    //
    //    System.out.println(e1.map2_2(Right(2))(_ + _))
    //    System.out.println(e2.map2_2(Right(2))(_ + _))

    val l1: List[Int] = List(1, 2, 3, 4, 0, 6, 7, 8)
    val l2: List[Either[String, Int]] = List(Right(1), Right(2), Left("Nope!"), Right(4), Right(5))

    System.out.println(Either.traverse(l1)(aa => Either.Try(Right((aa * 2) / aa))))
    System.out.println(Either.sequence(l2))
  }
}
