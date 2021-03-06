package mdparse

import mdparse.MdItem._
import org.scalatest.{FunSpec, Matchers}

class MarkdownSpec extends FunSpec with Matchers {

  it("should return html") {
    val md = Markdown(Seq(
      Header(1, Seq(Common("Header1"))),
      ThBreak,
      Header(4, Seq(Common("Header2"))),
      Paragraph(Seq(
        Common("paragpraph <sfaa dsfdsf"),
        Common("second"),
        Common("third "), Link("asdsad", "asdsad", None)
      )),
      Paragraph(Seq(
        Common("ssssssssssss")
      )),
      UnorderedList(Seq(
        ListItem(Seq(Common("item1"))),
        ListItem(Seq(Common("item 2"))))
      )
    ))

    val html = md.toHtml
    // TODO!
    val s = HtmlRender.Compact.render(html)
    val expect =
      """<h1>Header1</h1>
        |<th/>
        |<h4>Header2</h4>
        |<p>
        | paragpraph <sfaa dsfdsf
        | second
        | third
        | <a>asdsad</a>
        |</p>
        |<p>
        | ssssssssssss
        |</p>
        |<ul>
        | <li>
        |  item1
        | </li>
        | <li>
        |  item 2
        | </li>
        |</ul>
      """.stripMargin
    s.replaceAll("\n", "").replaceAll(" ", "") shouldBe expect.replaceAll("\n", "").replaceAll(" ", "")
  }
}
