package mdparse

import fastparse.all._
import mdparse.md._

object MdParser {

  val space = P ( " " )
  val newline = P ( "\n" | "\r\n" | "\r" | "\f" )

  val symbol = P ( !newline ~ AnyChar )

  val text = symbol.rep.!

  val paragraph = P ( text ).map(Paragraph)

  val header = P ( "#".rep(max = 6).! ~ space ~ text)
      .map({case (s, t) => Header(s.length, t)})

  val break = P ( space.rep(max = 3).? ~ ("*" | "_" | "-").rep(min = 3)).map(_ => ThematicBreak)

  val line = P( (header | break | paragraph) ~ newline | End )

  val lines = P( line.rep )

}
