package mdparse

import mdparse.md.MdItem


object md {

  sealed trait MdItem extends Product with Serializable

  sealed trait MdLeaf extends MdItem
  sealed trait MdNode extends MdItem

  final case class Header(level: Int, text: String) extends MdLeaf
  case object ThBreak extends MdLeaf

  final case class Paragraph(items: Seq[MdItem]) extends MdItem
  object Paragraph {
    def withItems(items: MdItem*): Paragraph = Paragraph(items)
  }

  sealed trait TextItem extends MdLeaf

  final case class Link(text: String, destination: String) extends TextItem
  object Link {
    def apply(link: String): Link = new Link(link, link)
  }
  final case class Image(text: String, destination: String, alt: String) extends TextItem

  final case class Common(s: String) extends TextItem
  final case class Strong(elems: Seq[TextItem]) extends TextItem
  final case class Italic(elems: Seq[TextItem]) extends TextItem
  final case class Code(s: String) extends TextItem

  final case class Text(items: Seq[TextItem]) extends MdLeaf

  final case class ListItem(items: Seq[MdItem]) extends MdNode

  sealed trait MdList extends MdNode
  final case class UnorderedList(items: Seq[ListItem]) extends MdList
  final case class OrderedList(items: Seq[ListItem]) extends MdList

  final case class RawHtml(node: HtmlTag) extends MdLeaf

  final case class FencedCode(lang: Option[String], data: String) extends MdLeaf
}

case class Markdown(items: Seq[MdItem]) {

  def toHtml: Seq[HtmlUnit] = {
    import md._
    import HtmlTags._

    def toHtml(item: MdItem): HtmlUnit = item match {
      case h: Header => header(h.level, h.text)
      case ThBreak => th
      case Paragraph(elems) => p(elems.map(toHtml))
      case Link(text, dest) => a(dest, text)
      case Image(text, dest, alt) => img(dest, text, alt)
      case Common(text) => innerBody(text)
      case Strong(elems) => strong(elems.map(toHtml))
      case Italic(elems) => italic(elems.map(toHtml))
      case Text(elems) => innerBody(elems.map(toHtml))
      case ListItem(elems) => li(elems.map(toHtml))
      case UnorderedList(elems) => ul(elems.map(toHtml))
      case OrderedList(elems) => ol(elems.map(toHtml))
      case RawHtml(node) => node
      case Code(s) => code(s)
      case FencedCode(lang, data) => pre(Seq(code(lang, data)))
    }
    items.map(toHtml)
  }
}
