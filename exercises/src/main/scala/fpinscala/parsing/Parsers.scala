package fpinscala.parsing

import language.higherKinds
import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self =>

  // Primitives:

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  def slice[A](p: Parser[A]): Parser[String]
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  // Implicit Converters

  implicit def regex(r: Regex): Parser[String]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps(p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  // Combinators

  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) | succeed(List())

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  def count[A](p: Parser[A]): Parser[Int] = many(p) map(_.size) //TODO: use slice

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if (n < 1) succeed(List())
    else map2(p, listOfN(n -1, p))(_ :: _)
  }

  def map[A,B](p: Parser[A])(f: A => B): Parser[B] = p flatMap(a => succeed(f(a)))

  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = //p.flatMap(a => p2.map(b => (a,b)))
    for { a <- p; b <- p2 } yield (a,b)

  def map2[A,B,C](p1: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = //product(p1, p2) map f.tupled
    for {a <- p1; b <- p2} yield f(a,b)

  def thatMany(toFind: String, input: String): Parser[String] =
    //"[0-9]".r flatMap(d => listOfN(d.toInt, toFind)) flatMap(cs => cs.mkString)
    for { d <- "[0-9]".r; l <- listOfN(d.toInt, toFind) } yield l.mkString

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def product[B>:A](p2: => Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def **[B>:A](p2: => Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def map2[B,C](p2: => Parser[B])(f: (A,B) => C): Parser[C] = self.map2(p,p2)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def slice = self.slice(p)
  }
}

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}