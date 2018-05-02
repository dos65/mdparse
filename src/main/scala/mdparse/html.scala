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
  val hr = inst("hr").empty
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
  val em = inst("em")
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

