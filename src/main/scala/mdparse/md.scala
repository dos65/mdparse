package mdparse

import mdparse.MdItem.Reference

sealed trait MdItem extends Serializable
trait SpanItem extends MdItem
trait BlockItem extends MdItem

object MdItem {

  final case class Link(text: String, destination: String, title: Option[String]) extends SpanItem
  final case class Image(destination: String, alt: String, title: Option[String]) extends SpanItem

  sealed trait ResolveType
  case object MaybeLink extends ResolveType
  case object MaybeImage extends ResolveType
  case class ResolveHandles(handles: Seq[Seq[SpanItem]], `type`: ResolveType) {
    def cleanedItem(item: SpanItem): String = item match {
      case Common(s) => s
      case Strong(elems) => elems.map(cleanedItem).mkString
      case Italic(elems) => elems.map(cleanedItem).mkString
      case Code(s) => s
      case Image(_, alt, _) => alt
      case Link(text, _, _) => text
    }

    def defaultItem(t: SpanItem): String = t match {
      case Common(s) => s
      case Strong(elems) => "_" + elems.map(defaultItem).mkString("") + "_"
      case Italic(elems) => "*" + elems.map(defaultItem).mkString("") + "*"
      case Code(s) => s
      case Image(_, alt, _) => alt
      case Link(text, _, _) => text
    }

    def partHandle(part: Seq[SpanItem])(f: SpanItem => String) = part.map(f).mkString
    def cleanedHandle(part: Seq[SpanItem]) = partHandle(part)(cleanedItem)
    def defaultHandle(part: Seq[SpanItem]) = partHandle(part)(defaultItem)

    def possibleHandles: Set[String] = {
      handles.flatMap(part => {
        val default = defaultHandle(part)
        val cleaned = cleanedHandle(part)
        Set(default, cleaned, cleaned.toLowerCase, cleaned.toUpperCase)
      }).toSet
    }
  }

  final class Common(s: String, resolveAs: Option[ResolveHandles]) extends SpanItem {
    def value: String = s
    def canResolve: Boolean = resolveAs.nonEmpty
    def resolve: Option[ResolveHandles] = resolveAs
  }
  object Common {
    def unapply(arg: Common): Option[String] = Some(arg.value)
    def apply(s: String): Common = new Common(s, None)
    def apply(s: String, resolveAs: ResolveHandles): Common = new Common(s, Option(resolveAs))
  }

  final case class Strong(items: Seq[SpanItem]) extends SpanItem
  final case class Italic(items: Seq[SpanItem]) extends SpanItem
  final case class Code(s: String) extends SpanItem

  final case class Reference(ref: String, destination: String, title: Option[String])

  case object ThBreak extends BlockItem
  final case class Header(level: Int, text: Seq[SpanItem]) extends BlockItem
  final case class Paragraph(items: Seq[SpanItem]) extends BlockItem
  final case class ListItem(items: Seq[MdItem]) extends BlockItem
  sealed trait MdList extends BlockItem
  final case class UnorderedList(items: Seq[ListItem]) extends MdList
  final case class OrderedList(items: Seq[ListItem]) extends MdList
  final case class RawHtml(node: HtmlTag2) extends BlockItem
  final case class FencedCode(lang: Option[String], data: String) extends BlockItem

}

import MdItem._

case class RawMarkdown(items: Seq[BlockItem], refs: Seq[Reference]) {

  def resolve(): Markdown = {

    val references = refs.map(ref => ref.ref -> ref).toMap

    def resolveCommon(value: String, handles: ResolveHandles): SpanItem = {
      handles.possibleHandles.flatMap(references.get).headOption match {
        case Some(ref) => handles.`type` match {
          case MaybeImage => Image(ref.destination, ref.ref, ref.title)
          case MaybeLink => Link(ref.ref, ref.destination, ref.title)
        }
        case None => Common(value)
      }
    }

    def resolveSpan(span: SpanItem): SpanItem = span match {
      case l: Link => l
      case i: Image => i
      case c: Code => c
      case c @ Common(value) => c.resolve match {
        case None => c
        case Some(handles) => resolveCommon(value, handles)
      }
      case s: Strong => Strong(s.items.map(resolveSpan))
      case i: Italic => Italic(i.items.map(resolveSpan))
    }

    def resolveListItem(l: ListItem): ListItem = {
      val x = items.map({
        case s: SpanItem => resolveSpan(s)
        case b: BlockItem => resolveBlock(b)
      })
      ListItem(x)
    }

    def resolveBlock(block: BlockItem): BlockItem = block match {
      case ThBreak => ThBreak
      case r: RawHtml => r
      case f: FencedCode => f
      case Header(l, text) => Header(l, text.map(resolveSpan))
      case Paragraph(items) => Paragraph(items.map(resolveSpan))
      case li: ListItem => resolveListItem(li)
      case UnorderedList(items) => UnorderedList(items.map(resolveListItem))
      case OrderedList(items) => OrderedList(items.map(resolveListItem))
    }

    val resolved = items.map(resolveBlock)
    Markdown(resolved)
  }
}

case class Markdown(items: Seq[BlockItem]) {

  import MdItem._

  def toHtml: Html = {
    import HtmlTags2._

    def toHtml(item: MdItem): HtmlUnit2 = item match {
      case h: Header => header(h.level).body(h.text.map(toHtml))
      case ThBreak => th
      case Paragraph(elems) => p.body(elems.map(toHtml))
      // TODO
      case Link(text, dest, title) => a(dest, text)
      case Image(dest, alt, title) => img(dest, alt, title)
      case Common(text) => InnerText(text)
      case Strong(elems) => strong.body(elems.map(toHtml))
      case Italic(elems) => em.body(elems.map(toHtml))
      case ListItem(elems) => li.body(elems.map(toHtml))
      case UnorderedList(elems) => ul.body(elems.map(toHtml))
      case OrderedList(elems) => ol.body(elems.map(toHtml))
      case RawHtml(node) => node
      case Code(s) => code.inner(s)
      case FencedCode(lang, data) => pre.body(code(data, lang))
      case x => throw new RuntimeException(s"Unexpected Element: $x: $items")
    }
    Html(items.map(toHtml))
  }
}
