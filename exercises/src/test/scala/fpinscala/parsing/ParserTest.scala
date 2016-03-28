package fpinscala.parsing

import org.scalacheck.Prop._
import org.scalacheck.Properties

/**
  * Tests for {@link Parsers}
  */
object ParserTest extends Properties("Parsers") {

  property("equality") = forAll {
    (s: String) => {
      //TODO
      //      val p1: Parser
    }
  }

  //run(char(c))(c.toString) == Right(c)

  //run(string(s))(s) == Right(s)

  //run(or(string("abra"),string("cadabra")))("abra") == Right("abra")
  //run(or(string("abra"),string("cadabra")))("cadabra") == Right("cadabra")

  //run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
  //run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
  //run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")

  /*
    def equal[A](p1: P[A], p2: P[A])(in: String) =
      forAll(in)(s => run(p1)(s) == run(p2)(s))
   */

  //equal(p, p.map(a => a))(in)

  //run(slice(('a'|'b').many))("aaba") == Right("aaba")

  //run(succeed(a))(s) == Right(a)
}
