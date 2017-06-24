package mdparse

import fastparse.all._
import fastparse.core.Logger._
import fastparse.noApi
import fastparse.utils.ReprOps
import mdparse.md._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer


trait MdParser extends Basic{

  val header = {
    val sharps = P ( "#".rep(min = 1, max = 6).! ).map(_.length)

    val readLine = P(CharsWhile(c => c != '\n' && c != '\r').!)
    val text = P(!ln ~ readLine.! ~/ lnOrEnd).map(_.trim)

    P( sharps ~ space ~/ text ).map({case (level, text) => Header(level, text)})
  }

  val break = {
    val spaces = P( space.rep(max = 3) )

    val stars = P( "*".rep(min = 3) )
    val underscores = P( "_".rep(min = 3) )
    val dashes = P( "-".rep(min = 3) )

    val breaks = P( blankLine.rep(min = 2) )

    val symbolic = P((dashes | stars | underscores) ~ space.rep ~ lnOrEnd)
    P((spaces.? ~ symbolic) | breaks).map(_ => Break)
  }

  val paragraph = {
//    val line = P((!ln ~ AnyChar.!).rep(1) ~ ln?)
//    line.map()
//    line.rep(1).map(text => {
//      TextItemsParser.text.parse(text) match {
//        case f: Parsed.Failure => throw new RuntimeException("ssasai")
//        case s: Parsed.Success[Text] => s.value
//      }
//    })
    P((!ln ~ AnyChar.!).rep(1) ~ lnOrEnd)
      .map(text => {
        val x = text.foldLeft(new StringBuilder)(_ append _).toString
        println(x)
        TextItemsParser.text.parse(x) match {
          case f: Parsed.Failure =>
            println(f.msg, f.index)
            throw new RuntimeException("ssasai")
          case s: Parsed.Success[Text] => s.value
        }
      }).rep(1)
      .map(lines => Paragraph(lines))

  }

  val list = {
    def mkUnordered(prefix: String): P[UnorderedList] = {
      P(prefix ~ space ~ (!lnOrEnd ~ AnyChar).rep(1).! ~ lnOrEnd)
        .map(s => ListItem(Seq(Common(s))))
        .rep(1)
        .map(items => UnorderedList(items))
    }

    val ordered = {
      val prefix = P( CharIn('0' to '9').rep(1) ~ "." )
      P(prefix ~ space ~ (!lnOrEnd ~ AnyChar).rep(1).! ~ lnOrEnd)
        .map(s => ListItem(Seq(Common(s))))
        .rep(1)
        .map(items => OrderedList(items))
    }

    val unordered = Seq("-", "+", "*").map(mkUnordered).reduce(_ | _)
    P(unordered | ordered)
  }

  val markdown: P[Seq[MdItem]] = P((header | break | blankLine | paragraph ).rep ~ End)
    .map(_.collect({case b: MdItem => b}))

}

object MdParser extends MdParser
