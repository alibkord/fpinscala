package fpinscala.parsing

import scala.language.{higherKinds, implicitConversions}

/**
  *
  * @tparam PE a Parse Error type
  * @tparam P  a Parser type
  */
trait Parsers[PE, P[+ _]] {
  self =>

  def run[A](p: P[A])(input: String): Either[PE, A]

  def char(c: Char): P[Char] = {
    val strP: P[String] = string(c.toString)
    strP.map(_.charAt(0))
  }

  def succeed[A](a: A): P[A] = string("").map(_ => a)

  def or[A](p1: P[A], p2: P[A]): P[A]

  def listOfN[A](n: Int, p: P[A]): P[List[A]]

  /**
    * @return zero or more instances of A found using the given parser p
    */
//  def many[A](p: P[A]): P[List[A]]

  def map[A, B](p: P[A])(f: A => B): P[B]

  def count[A](p: P[A]): P[Int] = map(many(p))(_.size)

  def many1[A](p: P[A]): P[List[A]]

  def product[A,B](p: P[A], p2: P[B]): P[(A, B)]

  /**
    * @return the portion of the input string examined by the parser, if successful
    */
  def slice[A](p: P[A]): P[String]

  def map2[A,B,C](p: P[A], p2: P[B])(f: (A,B) => C): P[C] = ParserOps(product(p, p2)).map(ab => f(ab._1,ab._2))

  //or, map2, succeed
  def many[A](p: P[A]): P[List[A]] = map2(p, many(p))(_ :: _) | succeed(List())

  // Implicit conversions

  implicit def string(s: String): P[String]
  implicit def asStringParser[A](a: A)(implicit f: A => P[String]): ParserOps[String] = ParserOps(f(a))
  implicit def operators[A](p: P[A]): ParserOps[A] = ParserOps(p)

  /**
    * Parser Operations
    */
  case class ParserOps[A](p: P[A]) {
    def |[B >: A](p2: P[B]): P[B] = self.or(p, p2)
    def or[B >: A](p2: => P[B]): P[B] = self.or(p, p2)
    def slice = self.slice(p)
    def map[B](f: A => B): P[B] = self.map(p)(f)
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""
}

case class ParseError(stack: List[(Location, String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}
