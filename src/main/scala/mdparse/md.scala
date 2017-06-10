package mdparse

object md {

  sealed trait MdItem extends Product with Serializable

  sealed trait MdLeaf extends MdItem
  sealed trait MdNode extends MdItem

  case class Header(level: Int, text: String) extends MdLeaf
  case object Break extends MdLeaf

  case class Paragraph(items: Seq[MdItem]) extends MdItem
  object Paragraph {
    def withItems(items: MdItem*): Paragraph = Paragraph(items)
  }

  case class Link(text: String, destination: String) extends MdLeaf
  object Link {
    def apply(link: String): Link = new Link(link, link)
  }
  case class RawText(s: String) extends MdLeaf
  case class BoldText(s: String) extends MdLeaf
  case class Italic(s: String) extends MdLeaf

}
