package mdparse.parser

import fastparse.all._
import mdparse.{Markdown, md}
import mdparse.md._
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.{FunSpec, Matchers}

class MdParserSpec extends FunSpec with Matchers {

  import MdParser._

  describe("header") {

    it("should parse") {
      header.parse("### Hello ") should parseTo(Header(3, "Hello"))
    }

  }

  describe("Link") {

    it("should parse") {
      TextItemsParser.link.parse("<hello>") should parseTo(Link("hello"))
      TextItemsParser.link.parse("[foo](/bar)") should parseTo(Link("foo", "/bar"))
    }
  }

  describe("Image") {

    it("should parse") {
      TextItemsParser.image.parse("![foo](bar)") should parseTo(Image("foo", "bar", "foo"))
    }
  }

  describe("th break") {

    it("should parse") {
      thBreak.parse("***") should parseTo(ThBreak)
      thBreak.parse("******") should parseTo(ThBreak)
      thBreak.parse("---") should parseTo(ThBreak)
      thBreak.parse("___") should parseTo(ThBreak)
      thBreak.parse("   ___") should parseTo(ThBreak)
    }
  }

  describe("p") {

    it("should parse") {
      val p =
        """first
          |second
          |and last""".stripMargin

      val expected = Paragraph.withItems(
        Text(Seq(Common("first"))),
        Text(Seq(Common("second"))),
        Text(Seq(Common("and last")))
      )
      paragraph.parse(p) should parseTo(expected)
    }

    it("with link") {
      val p =
        """first [foo](/bar)
          |second""".stripMargin

      val expected = Paragraph.withItems(
        Text(Seq(Common("first "), Link("foo", "/bar"))),
        Text(Seq(Common("second")))
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
           |        $s third-level-1
           |        $s third-level-2
           |    $s inner-2
           |$s third""".stripMargin
      }
      val expected = UnorderedList(Seq(
        ListItem(Seq(Text(Seq(Common("first"))))),
        ListItem(Seq(
          Text(Seq(Common("second")) ),
          UnorderedList(Seq(
            ListItem(Seq(
              Text(Seq(Common("inner-1"))),
              Text(Seq(Common("dsfdsfdsfsd"))),
              Text(Seq(Common("dsfsdfsdfsdfs"))),
              UnorderedList(Seq(
                ListItem(Seq(Text(Seq(Common("third-level-1"))))),
                ListItem(Seq(Text(Seq(Common("third-level-2")))))
              ))
            )),
            ListItem(Seq(
              Text(Seq(Common("inner-2")))
            ))
          ))
        )),
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
          |3. third""".stripMargin

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
          |ssssssssssss
          |
          |- item1
          |- item 2""".stripMargin

      val r = markdown.parse(text)
      r should parseTo(
        Markdown(Seq(
          Header(1, "Header1"),
          ThBreak,
          Header(4, "Header2"),
          Paragraph.withItems(
            Text(Seq(Common("paragpraph <sfaa dsfdsf"))),
            Text(Seq(Common("second"))),
            Text(Seq(Common("third "), Link("asdsad")))
          ),
          Paragraph.withItems(
            Text(Seq(Common("ssssssssssss")))
          ),
          UnorderedList(Seq(
            ListItem(Seq(Text(Seq(Common("item1"))))),
            ListItem(Seq(Text(Seq(Common("item 2")))))
          ))
        ))
      )
    }

  }

  class ParserMatcher(a: Any) extends Matcher[Parsed[_]] {
    import DebugPrinter._

    override def apply(left: Parsed[_]): MatchResult = {
      val result = left match {
        case s: Parsed.Success[_] => s.value == a
        case f: Parsed.Failure => false
      }

      def prettyS(x: Any): String = x match {
        case Parsed.Success(v, _) => prettyS(v)
        case pr: Product => pr.printDebug
        case any => any.toString
      }

      val rightString = prettyS(a)
      val leftString = prettyS(left)

      MatchResult(
        result,
        s"Parsed:\n $leftString\ndidn't match with\n $rightString",
        s"Parsed:\n $leftString\nis match with\n $rightString"
      )
    }

  }

  def parseTo[A](a: A): ParserMatcher = new ParserMatcher(a)

}
