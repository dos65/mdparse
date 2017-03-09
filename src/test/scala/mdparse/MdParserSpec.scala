package mdparse

import org.scalatest.{FlatSpec, FunSpec, Matchers}
import fastparse.all._
import mdparse.md.{Header, ThematicBreak}
import org.scalatest.matchers.{MatchResult, Matcher}

class MdParserSpec extends FunSpec with Matchers {

  import MdParser._

  describe("header") {

    it("should parse") {
      header.parse("### Hello ") should parseTo(Header(3, "Hello "))
    }

  }

  describe("th break") {

    it("should parse") {
      break.parse("***") should parseTo(ThematicBreak)
      break.parse("*****") should parseTo(ThematicBreak)
      break.parse("---") should parseTo(ThematicBreak)
      break.parse("___") should parseTo(ThematicBreak)
    }

  }

  class ParserMatcher[A](a: A) extends Matcher[Parsed[A]] {
    override def apply(left: Parsed[A]): MatchResult = {
      val result = left match {
        case s: Parsed.Success[A] => s.value == a
        case f: Parsed.Failure => false
      }
      MatchResult(
        result,
        s"$left didn't match with $a",
        s"$left is match with a"
      )
    }
  }

  def parseTo[A](a: A):ParserMatcher[A] = new ParserMatcher[A](a)

}
