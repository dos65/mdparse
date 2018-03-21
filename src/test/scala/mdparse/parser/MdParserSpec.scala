package mdparse.parser

import fastparse.all._
import mdparse._
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
      val s =
        """![foo *bar*]
          |
          |[foo *bar*]: train.jpg "train & tracks"""".stripMargin
      println(TextItemsParser.image.parse(s))
      TextItemsParser.image.parse("![foo](bar)") should parseTo(Image("bar", "foo", None))
    }

    it("multiline - second last"){
      val s =
        """![foo][bar]
          |
          |[bar]: /url""".stripMargin
      println(TextItemsParser.image.parse(s))
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
        Common("first"),
        Common("second"),
        Common("and last")
      )
      paragraph.parse(p) should parseTo(expected)
    }

    it("with link") {
      val p =
        """first [foo](/bar)
          |second""".stripMargin

      val expected = Paragraph.withItems(
        Common("first "), Link("foo", "/bar"),
        Common("second")
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
      val expected = Seq(
        Common("11sdd "),
        Italic(Seq(Common("three"))),
        Common(" one two "),
        Strong(Seq(
          Common("fourth "),
          Link("title", "link")
        ))
      )
      val r = TextItemsParser.text.parse(p)
      r should parseTo(expected)
    }

    it("should parse code") {
      val p = "`sudo rm -rf`"
      val r = TextItemsParser.text.parse(p)
      r should parseTo(Seq(Code("sudo rm -rf")))
    }
  }

  describe("fenced code") {

    it("should parse") {
      val s =
        """```scala
          |def x(a: Int) = a + 1
          |def x2(): Unit = ???
          |```
        """.stripMargin

      val expected = FencedCode(
        lang = Some("scala"),
        data =
          s"""def x(a: Int) = a + 1
             |def x2(): Unit = ???""".stripMargin
      )
      MdParser.fencedCode.parse(s) should parseTo(expected)
    }
  }

  describe("p2") {

    it("basic example") {
      val p = "hello dasd **sadasd** \n asdsadsadrewr sa asr "
      val expected = Paragraph(Seq(
        Common("hello dasd "), Strong(Seq( Common("sadasd") )), Common(" "),
        Common(" asdsadsadrewr sa asr ")
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
        ListItem(Seq(Common("first"))),
        ListItem(Seq(
          Common("second"),
          UnorderedList(Seq(
            ListItem(Seq(
              Common("inner-1"),
              Common("dsfdsfdsfsd"),
              Common("dsfsdfsdfsdfs"),
              UnorderedList(Seq(
                ListItem(Seq(Common("third-level-1"))),
                ListItem(Seq(Common("third-level-2")))
              ))
            )),
            ListItem(Seq(
              Common("inner-2"))
            ))
          ))
        ),
        ListItem(Seq(Common("third")))
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
        ListItem(Seq(Common("first"))),
        ListItem(Seq(Common("second"))),
        ListItem(Seq(Common("third")))
      ))

      list.parse(p) should parseTo(expected)
    }

    it("should respect tabs") {
      val s =
        "* first\n" +
          "\t* second\n" +
          "\t* third"

      val r = MdParser.list.parse(s)
      val expected = UnorderedList(Seq(
        ListItem(Seq(
          Common("first"),
          UnorderedList(Seq(
              ListItem(Seq(Common("second"))),
              ListItem(Seq(Common("third")))
          ))
        )
      )))
      r should parseTo(expected)
    }

    it("respect inner") {
      val s =
        """- a
          |  * b
          |  * c
          |    - d
          |- XXXX
        """.stripMargin
      val r = MdParser.list.parse(s)
      println(r)
    }

    it("case 2") {
      val s =
        """* 00:00:30 - Интро про Александра, типы, химия, лиспы, идрисы
          |    - Z
          |        - sdsd
          |    - ASoftware Foundations (1)ttpssoftwarefoundations.cis.upenn.edu
          |    - [Software Foundations (2)](https://github.com/idris-hackers/software-foundations)
          |    - [Type driven development with Idris](https://www.manning.com/books/type-driven-development-with-idris)
          |    - [Linear Algebra via Exterior Products](https://sites.google.com/site/winitzki/linalg)
          |* [Блог Виницкого](http://chaource.livejournal.com/)""".stripMargin
      println(MdParser.list.parse(s))
    }

    it("wtf") {
      import fastparse.all._

//      val x = P(" ".rep(exactly = 2))
//      val y = P(" ".rep(exactly = 4))
//
//      val or = P(y | x)
//      val sus = P(or ~ "-")
//      println(sus.parse("  -"))
//      println()
//      println(sus.parse("     -"))
//      println()
//      println(sus.parse("    -"))
//      println()


      val prefix = new parser.MdParser.ListPrefix(P("-"), UnorderedList.apply)

      println(prefix.parser(0).parse("-"))
      println(prefix.parser(1).parse("    -"))
      println(prefix.parser(1).parse("  -"))
      println(prefix.parser(2).parse("    -"))
    }
  }

  describe("html") {

    it("should parse self closed tags") {
      HtmlParser.html.parse("<br/>") should parseTo(RawHtml(HtmlTags2.br))
    }

    it("should parse tag") {
      HtmlParser.html.parse("<div></div>") should parseTo(RawHtml(HtmlTags2.div.empty))
    }

    it("should parse tag with attrs") {
      val expected = HtmlTags2.a(Seq("target" -> "blank"), Seq.empty)
      HtmlParser.html.parse("<a target=\"blank\"></a>") should parseTo(RawHtml(expected))
    }

    it("should parse tag with innerbody") {
      val html =
        """<div attr="value">
          |   <ul>
          |      <li><a href="link">yoyo</a></li>
          |   </ul>
          |   <p>
          |   sadasd
          |   sdasd sadasd sdasd
          |   </p>
          |   <br/>
          |</div>
        """.stripMargin
      val result = HtmlParser.html.parse(html)
      // TODO
//      println(result.get.value.node.toTextRepr.render)
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
          |<div>
          |  <a href="foo">foo</a>
          |</div>
          |
          |```scala
          |val a = "s"
          |```
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
            Common("paragpraph <sfaa dsfdsf"),
            Common("second"),
            Common("third "), Link("asdsad")
          ),
          Paragraph.withItems(
            Common("ssssssssssss")
          ),
          RawHtml(
            HtmlTags2.div.body(Seq(
              HtmlTags2.a("foo", "foo")
            ))
          ),
          FencedCode(Some("scala"), "val a = \"s\""),
          UnorderedList(Seq(
            ListItem(Seq(Common("item1"))),
            ListItem(Seq(Common("item 2")))
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
        case f: fastparse.core.Parsed.Failure[_, _] => f.msg
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
