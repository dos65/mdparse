package mdparse

import mdparse.md.MdItem

object md {

  sealed trait MdItem extends Product with Serializable

  final case class Header(level: Int, text: String) extends MdItem
  case object ThBreak extends MdItem

  final case class Paragraph(items: Seq[MdItem]) extends MdItem
  object Paragraph {
    def withItems(items: MdItem*): Paragraph = Paragraph(items)
  }

  sealed trait TextItem extends MdItem

  final case class Link(text: String, destination: String) extends TextItem
  object Link {
    def apply(link: String): Link = new Link(link, link)
  }
  final case class Image(destination: String, alt: String, title: Option[String]) extends TextItem

  final case class Common(s: String) extends TextItem
  final case class Strong(elems: Seq[TextItem]) extends TextItem
  final case class Italic(elems: Seq[TextItem]) extends TextItem
  final case class Code(s: String) extends TextItem
  final case class FencedCode(lang: Option[String], data: String) extends TextItem

  final case class ListItem(items: Seq[MdItem]) extends MdItem

  sealed trait MdList extends MdItem
  final case class UnorderedList(items: Seq[ListItem]) extends MdList
  final case class OrderedList(items: Seq[ListItem]) extends MdList

  final case class RawHtml(node: HtmlTag2) extends MdItem

}

case class UnresolvedMarkdown(items: Seq[MdItem])

case class Markdown(items: Seq[MdItem]) {

  def toHtml: Html = {
    import md._
    import HtmlTags2._

    def toHtml(item: MdItem): HtmlUnit2 = item match {
      case h: Header => header(h.level).inner(h.text)
      case ThBreak => th
      case Paragraph(elems) => p.body(elems.map(toHtml))
      case Link(text, dest) => a(dest, text)
      case Image(dest, alt, title) => img(dest, alt, title)
      case Common(text) => InnerText(text)
      case Strong(elems) => strong.body(elems.map(toHtml))
      case Italic(elems) => italic.body(elems.map(toHtml))
      case ListItem(elems) => li.body(elems.map(toHtml))
      case UnorderedList(elems) => ul.body(elems.map(toHtml))
      case OrderedList(elems) => ol.body(elems.map(toHtml))
      case RawHtml(node) => node
      case Code(s) => code.inner(s)
      case FencedCode(lang, data) => pre.body(code(data, lang))
    }
    Html(items.map(toHtml))
  }
}
