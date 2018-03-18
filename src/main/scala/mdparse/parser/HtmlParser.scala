package mdparse.parser

import fastparse.all._
import mdparse.{HtmlTag2, HtmlTags2}
import mdparse.md._

trait HtmlParser extends Basic {

  val html: P[RawHtml] = {
    val selfClosing: P[RawHtml] = P(wrappedBy("<", "/>").map(tag => RawHtml(HtmlTag2(tag, Seq.empty, Seq.empty))))

    def tag: P[RawHtml] = {

      val attributes = {
        val attrKey = P(!(ln | tab | space | "\"" | "=") ~ AnyChar).rep(1)
        val attrValue = P(!(ln | tab | space | "\"") ~ AnyChar).rep(1)
        val attribute = P(attrKey.! ~ "=\"" ~ attrValue.! ~ "\"")
        attribute.rep(min = 0, sep = " ".rep(1))
      }

      val name = P(!(ln | tab | space| ">") ~ AnyChar).rep(1)
      val start = P("<" ~ name.! ~ " ".? ~ attributes ~ " ".rep(0) ~ ">")
      def end(n: String) = P("</" ~ n ~ ">" ~/)

      start.flatMap({case (n, attrs) =>
        val ending = end(n)
        val x = P((!ending ~ (tag | ln | tab | AnyChar.!)).rep(0) ~ ending ~ (ln | tab).rep(0))
        val z = x.map(seq =>
          charsToBuilders(seq.filter(_ != ())).map {
            case s: StringBuilder => s.toString().replaceAll("\n", "").replaceAll("\r\n", "").trim
            case x => x
          }.collect {
            case s: String if s.nonEmpty => HtmlTags2.innerText(s)
            case r: RawHtml => r.node
          }
        )
        z.map(inner => RawHtml(HtmlTag2(n, attrs, inner)))
      })
    }
    selfClosing | tag
  }
}

object HtmlParser extends HtmlParser
