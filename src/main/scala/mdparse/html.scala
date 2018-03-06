package mdparse


sealed trait HtmlUnit {
  def toTextRepr: TextRepr
}

trait HtmlTag extends HtmlUnit

object HtmlUnit {

  type Attrs = Seq[(String, String)]

  final case class InnerTextBody(value: String) extends HtmlUnit {
    def toTextRepr: TextRepr = TextRepr.inlined(value)
  }
  final case class InnerBody(elems: Seq[HtmlUnit]) extends HtmlUnit {
    def toTextRepr: TextRepr = TextRepr.lines(elems.map(_.toTextRepr))
  }

  final case class InlineTag(name: String, attributes: Attrs, body: Seq[HtmlUnit]) extends HtmlTag {
    def toTextRepr: TextRepr = {
      import TextRepr._

      def attrs: String = attributes.foldLeft("")({case (acc, (k, v)) => acc + s""" $k="$v""""})
      def start: String = "<" + name + attrs + ">"
      def end: String = s"</$name>"

      inlined(start + body.map(_.toTextRepr.render(RenderOptions.inlined)).mkString(" ") + end)
    }
  }

  final case class Tag(name: String, attributes: Attrs, body: Seq[HtmlUnit]) extends HtmlTag {
    def toTextRepr: TextRepr = {
      import TextRepr._

      def attrs: String = attributes.foldLeft("")({case (acc, (k, v)) => acc + s""" $k="$v""""})
      def start: String = "<" + name + attrs + ">"
      def end: String = s"</$name>"

      lines(
        inlined(start) +: body.map(_.toTextRepr) :+ inlined(end)
      )
    }
  }

  final case class SelfClosingTag(name: String, attributes: Attrs) extends HtmlTag {
    def toTextRepr: TextRepr = {
      import TextRepr._

      def attrs: String = attributes.foldLeft("")({case (acc, (k, v)) => acc + s""" $k="$v""""})
      inlined("<" + name + attrs + "/>")
    }
  }

  def inlinedTag(name: String, attrs: Attrs, body: Seq[HtmlUnit]): HtmlTag = new InlineTag(name, attrs, body)
  def inlinedTag(name: String, body: Seq[HtmlUnit]): HtmlTag = new InlineTag(name, Seq.empty, body)
  def tag(name: String, attrs: Attrs, body: Seq[HtmlUnit]): HtmlTag = new Tag(name, attrs, body)
  def tag(name: String, body: Seq[HtmlUnit]): HtmlTag = new Tag(name, Seq.empty, body)
  def selfClosingTag(name: String, attrs: Attrs): HtmlTag = new SelfClosingTag(name, attrs)
  def selfClosingTag(name: String): HtmlTag = new SelfClosingTag(name, Seq.empty)

}

object HtmlTags {
  import HtmlUnit._

  def innerBody(s: String): HtmlUnit = new InnerTextBody(s)
  def innerBody(elems: Seq[HtmlUnit]): HtmlUnit = new InnerBody(elems)
  def header(level: Int, text: String, attrs: Attrs = Seq.empty): HtmlTag = inlinedTag(s"h$level", Seq(innerBody(text)))
  val th = selfClosingTag("th")
  def p(inner: Seq[HtmlUnit]): HtmlTag = tag("p", inner)
  def a(dest: String, text: String): HtmlTag = inlinedTag("a", Seq(innerBody(text)))
  def img(dest: String, text: String, alt: String): HtmlTag = inlinedTag("img", Seq("alt" -> alt), Seq(innerBody(text)))
  def strong(inner: Seq[HtmlUnit]): HtmlTag = inlinedTag("string", inner)
  def italic(inner: Seq[HtmlUnit]): HtmlTag = inlinedTag("italic", inner)
  def li(inner: Seq[HtmlUnit]): HtmlTag = tag("li", inner)
  def ul(inner: Seq[HtmlUnit]): HtmlTag = tag("ul", inner)
  def ol(inner: Seq[HtmlUnit]): HtmlTag = tag("ol", inner)
}
