package mdparse

object md {

  sealed trait Block

  case class Header(level: Int, text: String) extends Block
  case class Paragraph(text: String) extends Block
  case object ThematicBreak extends Block
}
