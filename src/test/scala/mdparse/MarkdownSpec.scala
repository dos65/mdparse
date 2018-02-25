package mdparse

import mdparse.md._
import org.scalatest.{FunSpec, Matchers}

class MarkdownSpec extends FunSpec with Matchers {

  it("should return html") {
    val md = Markdown(Seq(
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

    val html = md.toHtml
    val s = html.map(_.toTextRepr.render).mkString("\n")
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
