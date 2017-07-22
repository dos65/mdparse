package mdparse.parser

import fastparse.all._

trait Basic {

  val space = P( " " )
  val ln = StringIn("\r\n", "\n")
  val tab = StringIn("\t")
  val lnOrEnd = P(ln | End)

  val blankLine = P(space.rep ~ ln)

  def WhileNotIn(s: Char*): P0 = CharsWhile(c => !s.contains(c))
}

object Basic extends Basic
