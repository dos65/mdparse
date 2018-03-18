package mdparse


sealed trait HtmlUnit {
  def toTextRepr: TextRepr
}

trait HtmlUnit2
final case class InnerText(text: String) extends HtmlUnit2
final case class HtmlTag2(
  name: String,
  attributes: Seq[(String, String)],
  body: Seq[HtmlUnit2]
) extends HtmlUnit2

case class Html(elements: Seq[HtmlUnit2])

object HtmlTags2 {

  case class TagInst(name: String) {
    def apply(attributes: Seq[(String, String)], body: Seq[HtmlUnit2]): HtmlTag2 =
      HtmlTag2(name, attributes, body)

    def attrs(attributes: (String, String)*): HtmlTag2 = HtmlTag2(name, attributes, Seq.empty)

    def body(body: Seq[HtmlUnit2]): HtmlTag2 = HtmlTag2(name, Seq.empty, body)
    def body(body: HtmlUnit2): HtmlTag2 = HtmlTag2(name, Seq.empty, Seq(body))

    def inner(innerText: String): HtmlTag2 = HtmlTag2(name, Seq.empty, Seq(InnerText(innerText)))

    def empty: HtmlTag2 = HtmlTag2(name, Seq.empty, Seq.empty)
  }

  def inst(name: String): TagInst = TagInst(name)

  def tag(name: String): HtmlTag2 = HtmlTag2(name, Seq.empty, Seq.empty)

  def header(level: Int) = inst("h" + level.toString)
  val th = inst("th").empty
  val p = inst("p")
  val a = { new TagInst("a") {
    def apply(dest: String, text: String): HtmlTag2 =
      HtmlTag2(name, Seq("href" -> dest), Seq(InnerText(text)))
  }}

  val img = new TagInst("img") {
    def apply(dest: String, alt: String, title: Option[String]): HtmlTag2 =
      HtmlTag2(name, Seq("src" -> dest, "alt" -> alt) ++ title.map(v => "title" -> v), Seq.empty)
  }

  val strong = inst("strong")
  val italic = inst("italic")
  val li = inst("li")
  val ul = inst("ul")
  val ol = inst("ol")
  val pre = inst("pre")
  val code = new TagInst("code") {
    def apply(code: String, lang: Option[String]): HtmlTag2 = {
      val langClass = lang.fold(Seq.empty[(String, String)])(v => Seq("class" -> s"language-$v"))
      HtmlTag2(name, langClass, Seq(innerText(code)))
    }
  }

  val div = inst("div")
  val br = HtmlTag2("br", Seq.empty, Seq.empty)

  def innerText(s: String): InnerText = InnerText(s)

}

//trait HtmlTag extends HtmlUnit
//
//object HtmlUnit {
//
//  type Attrs = Seq[(String, String)]
//
//  final case class InnerTextBody(value: String) extends HtmlUnit {
//    def toTextRepr: TextRepr = TextRepr.inlined(value)
//  }
//  final case class InnerBody(elems: Seq[HtmlUnit]) extends HtmlUnit {
//    def toTextRepr: TextRepr = TextRepr.lines(elems.map(_.toTextRepr))
//  }
//
//  final case class InlineTag(name: String, attributes: Attrs, body: Seq[HtmlUnit]) extends HtmlTag {
//    def toTextRepr: TextRepr = {
//      import TextRepr._
//
//      def attrs: String = attributes.foldLeft("")({case (acc, (k, v)) => acc + s""" $k="$v""""})
//      def start: String = "<" + name + attrs + ">"
//      def end: String = s"</$name>"
//
//      inlined(start + body.map(_.toTextRepr.render(RenderOptions.inlined)).mkString(" ") + end)
//    }
//  }
//
//  final case class Tag(name: String, attributes: Attrs, body: Seq[HtmlUnit]) extends HtmlTag {
//    def toTextRepr: TextRepr = {
//      import TextRepr._
//
//      def attrs: String = attributes.foldLeft("")({case (acc, (k, v)) => acc + s""" $k="$v""""})
//      def start: String = "<" + name + attrs + ">"
//      def end: String = s"</$name>"
//
//      lines(
//        inlined(start) +: body.map(_.toTextRepr) :+ inlined(end)
//      )
//    }
//  }
//
//  final case class SelfClosingTag(name: String, attributes: Attrs) extends HtmlTag {
//    def toTextRepr: TextRepr = {
//      import TextRepr._
//
//      def attrs: String = attributes.foldLeft("")({case (acc, (k, v)) => acc + s""" $k="$v""""})
//      inlined("<" + name + attrs + "/>")
//    }
//  }
//
//  def inlinedTag(name: String, attrs: Attrs, body: Seq[HtmlUnit]): HtmlTag = new InlineTag(name, attrs, body)
//  def inlinedTag(name: String, body: Seq[HtmlUnit]): HtmlTag = new InlineTag(name, Seq.empty, body)
//  def tag(name: String, attrs: Attrs, body: Seq[HtmlUnit]): HtmlTag = new Tag(name, attrs, body)
//  def tag(name: String, body: Seq[HtmlUnit]): HtmlTag = new Tag(name, Seq.empty, body)
//  def selfClosingTag(name: String, attrs: Attrs): HtmlTag = new SelfClosingTag(name, attrs)
//  def selfClosingTag(name: String): HtmlTag = new SelfClosingTag(name, Seq.empty)
//
//}
//
//object HtmlTags {
//  import HtmlUnit._
//
//  def innerBody(s: String): HtmlUnit = new InnerTextBody(s)
//  def innerBody(elems: Seq[HtmlUnit]): HtmlUnit = new InnerBody(elems)
//  def header(level: Int, text: String, attrs: Attrs = Seq.empty): HtmlTag = inlinedTag(s"h$level", Seq(innerBody(text)))
//  val th = selfClosingTag("th")
//  def p(inner: Seq[HtmlUnit]): HtmlTag = tag("p", inner)
//  def a(dest: String, text: String): HtmlTag = inlinedTag("a", Seq(innerBody(text)))
//  def img(dest: String, text: String, alt: String): HtmlTag = inlinedTag("img", Seq("alt" -> alt, "src" -> dest), Seq.empty)
//  def strong(inner: Seq[HtmlUnit]): HtmlTag = inlinedTag("string", inner)
//  def italic(inner: Seq[HtmlUnit]): HtmlTag = inlinedTag("italic", inner)
//  def li(inner: Seq[HtmlUnit]): HtmlTag = tag("li", inner)
//  def ul(inner: Seq[HtmlUnit]): HtmlTag = tag("ul", inner)
//  def ol(inner: Seq[HtmlUnit]): HtmlTag = tag("ol", inner)
//  def code(data: String): HtmlTag = tag("code", Seq(innerBody(data)))
//  def code(lang: Option[String], data: String): HtmlTag = {
//    val langClass = lang.fold(Seq.empty[(String, String)])(v => Seq("class" -> s"language-$v"))
//    tag("code", langClass, Seq(innerBody(data)))
//  }
//  def pre(inner: Seq[HtmlUnit]): HtmlTag = tag("pre", inner)
//}
