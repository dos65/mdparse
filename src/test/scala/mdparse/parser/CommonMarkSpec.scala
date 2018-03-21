package mdparse.parser

import mdparse.HtmlRender
import org.scalatest.{FunSpec, Matchers}

import scala.io.Source

case class SpecTest(
  markdown: String,
  html: String,
  section: String,
  example: Int
)

class CommonMarkSpec extends FunSpec with Matchers with SpecsReader {

  val enabledSections = Seq(
    "Images"
  )

  specTests.groupBy(_.section)
    //.filter(x => enabledSections.contains(x._1))
    .foreach({case (section, tests) => {

      describe("Common marks section:" + section) {
        tests.zipWithIndex.foreach({case (t, index) => {
          it(s"num: $index example: ${t.example}: ${t.markdown}") { test(t) }
        }})
      }

  }})

  def test(specTest: SpecTest): Unit = {
    import fastparse.all._

    def unescape(s: String): String = {
      s.replaceAll("&quot;", "\"")
        .replaceAll("&amp;", "&")
    }
    def normalize(s: String): String = unescape(s.replaceAll("\n", ""))

    val in = specTest.markdown
    MdParser.markdown.parse(in) match {
      case Parsed.Success(r, _) =>
        val rendered = HtmlRender.Compact.render(r.toHtml)
        val out = normalize(rendered)
        val expected = normalize(specTest.html)
        if (out != expected) {
          fail(s"Failed: in:\n $in \n out: $out \n exp: $expected")
        }
//        if (normalize(out))
//        parserOut shouldBe specTest.html
      case f:Parsed.Failure =>
        fail(s"Couldn't parse: $in . reason: ${f.msg}")
    }
  }

}

trait SpecsReader {
  import io.circe._
  import io.circe.parser._
  import io.circe.generic.semiauto._

  implicit val specTestDecoder: Decoder[SpecTest] = deriveDecoder[SpecTest]

  def specTests: Seq[SpecTest] = {

    def rawData: String = {
      val stream = getClass.getClassLoader.getResourceAsStream ("commonmark_tests.json")
      Source.fromInputStream(stream).mkString
    }

   decode[List[SpecTest]](rawData) match {
     case Right(tests) => tests
     case Left(e) => throw e
   }
  }

}
