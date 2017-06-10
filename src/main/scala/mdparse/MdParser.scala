package mdparse

import fastparse.all._
import fastparse.core.Logger._
import fastparse.noApi
import mdparse.md._

import scala.collection.mutable.ArrayBuffer

object MdParser {

  val Space = P( " " )
  val Ln = P( StringIn("\r\n", "\n") )
  val LnOrEnd = P(Ln | End)

  val BlankLine = P(Space.rep ~ Ln)

  val HeaderP = {
    val Sharps = P ( "#".rep(min = 1, max = 6).! ).map(_.length)

    val ReadLine = P(CharsWhile(c => c != '\n' && c != '\r').!)
    val Text = P(!Ln ~ ReadLine.! ~/ LnOrEnd).map(_.trim)

    P( Sharps ~ Space ~/ Text ).map({case (level, text) => Header(level, text)})
  }

  val BreakP = {
    val Spaces = P( Space.rep(max = 3) )

    val Stars = P( "*".rep(min = 3) )
    val Underscores = P( "_".rep(min = 3) )
    val Dashes = P( "-".rep(min = 3) )

    val Breaks = P( Ln.rep(min = 2) )

    val Symbolic = P((Dashes | Stars | Underscores) ~ Space.rep ~ LnOrEnd)
    P(Spaces.? ~ (Symbolic | Breaks).!).map(_ => Break)
  }

  val LinkP = {
    val ShortForm = {
      val notAllowed = Seq('>', '<', ' ', '\t', '\n')
      P("<" ~ WhileNotIn(notAllowed: _*).! ~ ">").map(link => Link(link))
    }
    val LongForm = {
      val title = P("[" ~ WhileNotIn('[', ']', '\n').! ~ "]")
      val inBrackets = P("(" ~ WhileNotIn('(', ' ', '\t', ')', '\n').! ~ ")")
      P(title ~ inBrackets).map({case (title, dest) => Link(title, dest)})
    }
    P(ShortForm | LongForm)
  }

  val ParagraphP = {
    val MultiSpace = P( Space.rep(1) | "\n" | "\r\n" ).map(_ => " ")

    val Extractors = P( LinkP | MultiSpace | AnyChar.! )

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

    val Bold = {
      def wrappedBy(s: String): P0 = {
        P(s ~ (!s ~ AnyChar.rep(1).!) ~ s)
      }
      P(wrappedBy("**") | wrappedBy("__")).map(BoldText(_))
    }

    def postProcessText(t: RawText): Seq[MdItem] = {
      val parser = P( Bold.? )
      parser.parse(t.s)
    }

    P( !BreakP ~ Extractors.rep(min = 1) )
      .map(foldCharsToText)
      .map(items => Paragraph(items))
  }

  val markdown: P[Seq[MdItem]] = P((HeaderP | BreakP | ParagraphP ).rep ~ End)
    .map(_.collect({case b: MdItem => b}))

  private def WhileNotIn(s: Char*): P0 = CharsWhile(c => !s.contains(c))
}
