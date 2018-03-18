package mdparse.parser

import mdparse.HtmlRender
import org.scalatest.{FunSpec, Matchers}

class ManualCommonMarkSpec extends FunSpec with Matchers {

  describe("Image") {

//    spec(543, "![foo](/url \"title\")", "<p><img src=\"/url\" alt=\"foo\" title=\"title\" /></p>")
//    spec(544, )
  }

  def spec(specNum: Int, md: String, exp: String) = it(s"md spec: $md, exmaple: $specNum") {
    test(md, exp)
  }

  def test(
    md: String,
    exp: String
  ): Unit = {
    import fastparse.all._

    def unescape(s: String): String = {
      s.replaceAll("&quot", "\"")
        .replaceAll("&amp", "&")
    }
    def normalize(s: String): String = unescape(s.replaceAll("\n", ""))

    MdParser.markdown.parse(md) match {
      case Parsed.Success(r, _) =>
        val rendered = HtmlRender.Compact.render(r.toHtml)
        val out = normalize(rendered)
        val expected = normalize(exp)
        if (out != expected) {
          fail(s"Failed: in:\n $md \n out: $out \n exp: $expected")
        }
      case f:Parsed.Failure =>
        fail(s"Couldn't parse: $md . reason: ${f.msg}")
    }
  }
}
