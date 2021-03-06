package mdparse.parser

import java.io.{PrintWriter, StringWriter}

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
    "Tabs"//,
  //  "Images"
  )

  specTests.groupBy(_.section)
    //.filter(s => enabledSections.contains(s._1))
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
    val parsed = try {
      MdParser.parse(in)
    } catch {
      case e: Throwable =>
        val sw = new StringWriter()
        e.printStackTrace(new PrintWriter(sw))
        Left(s"Parse fatal error, ${e.getClass} ${e.getMessage} \n ${sw}")
    }

    parsed match {
      case Right(md) =>
        val rendered = HtmlRender.Compact.render(md.toHtml)
        val out = normalize(rendered)
        val expected = normalize(specTest.html)
        if (out != expected) {
          fail(s"Failed: in:\n $in \n out: $out \n exp: $expected")
        }
      case Left(msg) =>
        fail(s"Couldn't parse: $in . reason: $msg")
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
