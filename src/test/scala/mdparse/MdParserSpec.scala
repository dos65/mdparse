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

//    it("should parse") {
//      link.parse("<hello>") should parseTo(Link("hello"))
//      link.parse("[foo](/bar)") should parseTo(Link("foo", "/bar"))
//    }
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
        Common("first second and last")
      )
      paragraph.parse(p) should parseTo(expected)
    }

    it("with link") {
      val p =
        """first [foo](/bar)
          |second""".stripMargin

      val expected = Paragraph.withItems(
        Common("first "),
        Link("foo", "/bar"),
        Common(" second")
      )
      paragraph.parse(p) should parseTo(expected)
    }

    it("bold text") {
      val p1 = "**bold text**"
      val p2 = "__bold text__"

      val expected = Strong(Seq(Common("bold text")))

      TextItemsParser.strong.parse(p1) should parseTo(expected)
      TextItemsParser.strong.parse(p2) should parseTo(expected)
    }

    it("italic") {
      val p = "*italic text*"
      val expected = Italic(Seq(Common("italic text")))
      TextItemsParser.italic.parse(p) should parseTo(expected)
    }
  }

  describe("text") {

    it("should parse any text") {
      val p = "11sdd *three* one two **fourth [title](link)**"
      val expected = Text(Seq(
        Common("11sdd "),
        Italic(Seq(Common("three"))),
        Common(" one two "),
        Strong(Seq(
          Common("fourth "),
          Link("title", "link")
        ))
      ))
      val r = TextItemsParser.text.parse(p)
      println(r)
      r should parseTo(expected)
    }
  }

  describe("p2") {

    it("basic example") {
      val p = "hello dasd **sadasd** \n asdsadsadrewr sa asr "
      val expected = Paragraph(Seq(
        Text(Seq( Common("hello dasd "), Strong(Seq( Common("sadasd") )), Common(" ") )),
        Text(Seq( Common(" asdsadsadrewr sa asr ") ))
      ))
      val r = MdParser.paragraph.parse(p)
      r should parseTo(expected)
    }
  }

  describe("list") {

    it("unordered") {
      def mkString(s: String): String = {
        s"""$s first
           |$s second
           |    $s inner-1
           |    dsfdsfdsfsd
           |    dsfsdfsdfsdfs
           |    $s inner-2
           |$s third""".stripMargin
      }
      println(mkString("-"))
      val expected = UnorderedList(Seq(
        ListItem(Seq(Text(Seq(Common("first"))))),
        ListItem(Seq(Text(Seq(Common("second"))))),
        ListItem(Seq(Text(Seq(Common("third")))))
      ))
      list.parse(mkString("*")) should parseTo(expected)
      list.parse(mkString("-")) should parseTo(expected)
      list.parse(mkString("+")) should parseTo(expected)
    }

    it("ordered") {
      val p =
        """1. first
          |2. second
          |3. third
         """.stripMargin

      val expected = OrderedList(Seq(
        ListItem(Seq(Text(Seq(Common("first"))))),
        ListItem(Seq(Text(Seq(Common("second"))))),
        ListItem(Seq(Text(Seq(Common("third")))))
      ))

      list.parse(p) should parseTo(expected)
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

  class ParserMatcher(a: Any) extends Matcher[Parsed[_]] {
    override def apply(left: Parsed[_]): MatchResult = {
      val result = left match {
        case s: Parsed.Success[_] => s.value == a
        case f: Parsed.Failure => false
      }
      MatchResult(
        result,
        s"$left didn't match with $a",
        s"$left is match with a"
      )
    }
  }

  def parseTo[A](a: A): ParserMatcher = new ParserMatcher(a)

}
