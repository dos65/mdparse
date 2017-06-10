package mdparse

import org.scalatest.{FlatSpec, FunSpec, Matchers}
import fastparse.all._
import mdparse.md._
import org.scalatest.matchers.{MatchResult, Matcher}

class MdParserSpec extends FunSpec with Matchers {

  import MdParser._

  describe("header") {

    it("should parse") {
      header.parse("### Hello ") should parseTo(Header(3, "Hello"))
    }

  }

  describe("Link") {

    it("should parse") {
      link.parse("<hello>") should parseTo(Link("hello"))
      link.parse("[foo](/bar)") should parseTo(Link("foo", "/bar"))
    }
  }

  describe("th break") {

    it("should parse") {
      break.parse("***") should parseTo(Break)
      break.parse("******") should parseTo(Break)
      break.parse("---") should parseTo(Break)
      break.parse("___") should parseTo(Break)
      break.parse("   ___") should parseTo(Break)
    }
  }

  describe("p") {

    it("should parse") {
      val p =
        """first
          |second
          |and last""".stripMargin

      val expected = Paragraph.withItems(
        RawText("first second and last")
      )
      paragraph.parse(p) should parseTo(expected)
    }

    it("with link") {
      val p =
        """first [foo](/bar)
          |second""".stripMargin

      val expected = Paragraph.withItems(
        RawText("first "),
        Link("foo", "/bar"),
        RawText(" second")
      )
      paragraph.parse(p) should parseTo(expected)
    }

    it("bold text") {
      val p1 = "**bold text**"
      val p2 = "__bold text__"

      val expected = Paragraph.withItems(BoldText("bold text"))

      paragraph.parse(p1) should parseTo(expected)
      paragraph.parse(p2) should parseTo(expected)
    }

    it("italic") {
      val p = "*italic text*"
      val expected = Paragraph.withItems(Italic("italic text"))
      paragraph.parse(p) should parseTo(expected)
    }
  }

  describe("complex parsing") {

    it("should parse all") {
      val text = """# Header1
          |
          |----
          |#### Header2
          |
          |paragpraph <sfaa dsfdsf
          |second
          |third <asdsad>
          |
          |ssssssssssss""".stripMargin

      val r = markdown.parse(text)
      println(r)
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
