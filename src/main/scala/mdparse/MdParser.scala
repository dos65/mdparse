package mdparse

import fastparse.all._
import fastparse.core.Logger._
import fastparse.noApi
import mdparse.md._

import scala.collection.mutable.ArrayBuffer

object MdParser {

  val space = P( " " )
  val Ln = P( StringIn("\r\n", "\n") )
  val LnOrEnd = P(Ln | End)

  val BlankLine = P(space.rep ~ Ln)

  val header = {
    val sharps = P ( "#".rep(min = 1, max = 6).! ).map(_.length)

    val readLine = P(CharsWhile(c => c != '\n' && c != '\r').!)
    val text = P(!Ln ~ readLine.! ~/ LnOrEnd).map(_.trim)

    P( sharps ~ space ~/ text ).map({case (level, text) => Header(level, text)})
  }

  val break = {
    val spaces = P( space.rep(max = 3) )

    val stars = P( "*".rep(min = 3) )
    val underscores = P( "_".rep(min = 3) )
    val dashes = P( "-".rep(min = 3) )

    val breaks = P( BlankLine.rep(min = 2) )

    val symbolic = P((dashes | stars | underscores) ~ space.rep ~ LnOrEnd)
    P((spaces.? ~ symbolic) | breaks).map(_ => Break)
  }

  val link = {
    val shortForm = {
      val notAllowed = Seq('>', '<', ' ', '\t', '\n')
      P("<" ~ WhileNotIn(notAllowed: _*).! ~ ">").map(link => Link(link))
    }
    val longForm = {
      val title = P("[" ~ WhileNotIn('[', ']', '\n').! ~ "]")
      val inBrackets = P("(" ~ WhileNotIn('(', ' ', '\t', ')', '\n').! ~ ")")
      P(title ~ inBrackets).map({case (title, dest) => Link(title, dest)})
    }
    P(shortForm | longForm)
  }

  val paragraph = {
    def wrappedBy(s: String): P[String] = {
      P(s ~ (!s ~ AnyChar).rep(1).! ~ s)
    }

    val multiSpace = P( space.rep(1) | "\n" | "\r\n" ).map(_ => " ")
    val bold = P(wrappedBy("**") | wrappedBy("__")).map(BoldText(_))
    val italic = P(wrappedBy("*")).map(Italic)

    val extractors = P( link | bold | italic | multiSpace | AnyChar.! )

    def foldCharsToText(a: Seq[Any]): Seq[MdItem] = {
      a.foldLeft(ArrayBuffer.empty[MdItem]) {
        case (arr, s: String) =>
          arr.lastOption match {
            case Some(x: RawText) =>
              arr(arr.length - 1) = x.copy(x.s + s)
              arr
            case _ => arr :+ RawText(s)
          }
        case (seq, x: MdItem) => seq :+ x
        case (seq, x) => throw new IllegalArgumentException(s"Unexpected element $x")
      }
    }


    P( !break ~ extractors.rep(min = 1) )
      .map(foldCharsToText)
      .map(items => Paragraph(items))
  }

  val markdown: P[Seq[MdItem]] = P((header | break | BlankLine | paragraph ).rep ~ End)
    .map(_.collect({case b: MdItem => b}))

  private def WhileNotIn(s: Char*): P0 = CharsWhile(c => !s.contains(c))
}
