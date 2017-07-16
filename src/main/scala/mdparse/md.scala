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

  sealed trait TextItem extends MdLeaf

  case class Link(text: String, destination: String) extends TextItem
  object Link {
    def apply(link: String): Link = new Link(link, link)
  }

  case class Common(s: String) extends TextItem
  case class Strong(elems: Seq[TextItem]) extends TextItem
  case class Italic(elems: Seq[TextItem]) extends TextItem

  case class Text(items: Seq[TextItem]) extends MdLeaf

  case class ListItem(items: Seq[MdItem]) extends MdNode

  sealed trait MdList extends MdNode
  case class UnorderedList(items: Seq[ListItem]) extends MdList
  case class OrderedList(items: Seq[ListItem]) extends MdList

}
