package mdparse

import org.scalatest.{FlatSpec, FunSpec, Matchers}
import fastparse.all._
import mdparse.md.{Break, Header, Link, Paragraph, RawText}
import org.scalatest.matchers.{MatchResult, Matcher}

class MdParserSpec extends FunSpec with Matchers {

  import MdParser._

  describe("header") {

    it("should parse") {
      HeaderP.parse("### Hello ") should parseTo(Header(3, "Hello"))
    }

  }

  describe("Link") {

    it("should parse") {
      LinkP.parse("<hello>") should parseTo(Link("hello"))
      LinkP.parse("[foo](/bar)") should parseTo(Link("foo", "/bar"))
    }
  }

  describe("th break") {

    it("should parse") {
      BreakP.parse("***") should parseTo(Break)
      BreakP.parse("******") should parseTo(Break)
      BreakP.parse("---") should parseTo(Break)
      BreakP.parse("___") should parseTo(Break)
      BreakP.parse("   ___") should parseTo(Break)
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
      ParagraphP.parse(p) should parseTo(expected)
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
      ParagraphP.parse(p) should parseTo(expected)
    }

    it("with text bold") {
      val p = "abc **yoyo** asd"
      println(ParagraphP.parse(p))
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

    it("asdasd") {
      val text = "---- asdsadasd"
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
