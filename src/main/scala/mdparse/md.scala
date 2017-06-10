package mdparse

object md {

  sealed trait MdItem extends Product with Serializable

  case class Header(level: Int, text: String) extends MdItem
  case object Break extends MdItem

  case class Paragraph(items: Seq[MdItem]) extends MdItem
  object Paragraph {
    def withItems(items: MdItem*): Paragraph = Paragraph(items)
  }

  case class Link(text: String, destination: String) extends MdItem
  object Link {
    def apply(link: String): Link = new Link(link, link)
  }
  case class RawText(s: String) extends MdItem
  case class BoldText(s: String) extends MdItem

}
