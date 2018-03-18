package mdparse

case class RenderOptions(
  level: Int,
  step: Int,
  spaceSymbol: String,
  sepSymbol: String
) {
  def nextLevel: RenderOptions = copy(level = level + 1)
}

object RenderOptions {
  def default = RenderOptions(0, 4, " ", "\n")
  def inlined = RenderOptions(0, 1, " ", " ")
}

trait HtmlRender {
  val sep: String
  def render(unit: HtmlUnit2): String
  def render(html: Html): String = html.elements.map(render).mkString(sep)
}

object HtmlRender {

  val VoidElements = Set(
    "area", "base", "br", "col", "embed", "hr", "img", "input",
    "keygen", "link", "meta", "param", "source", "track", "wbr"
  )
  def isVoid(name: String): Boolean = VoidElements.contains(name)

  val Compact: HtmlRender = new HtmlRender {
    val sep: String = ""

    def render(unit: HtmlUnit2): String = {

      def mkAttrs(attrs: Seq[(String, String)]): String = {
        if (attrs.isEmpty) ""
        else attrs.map({case (k,v) =>  k + "=\"" + v + "\""}).mkString(" ", " ", " ")
      }

      unit match {
        case InnerText(text) => text
        case HtmlTag2(name, attrs, _) if isVoid(name) => s"<$name${mkAttrs(attrs)}/>"
        case HtmlTag2(name, attrs, body) =>
          val rBody = body.map(render).mkString(sep)
          s"<$name${mkAttrs(attrs)}>$rBody</$name>"
      }
    }

  }
}

