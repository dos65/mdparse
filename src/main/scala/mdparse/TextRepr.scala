package mdparse

sealed trait TextRepr {
  def render(options: RenderOptions): String
  def render: String = render(RenderOptions.default)
}

object TextRepr {

  case class Lines(values: Seq[TextRepr]) extends TextRepr {
    def render(options: RenderOptions): String =
      values.map({
        case l: Lines => l.render(options.nextLevel)
        case t: Text => t.render(options)
      }).mkString(options.sepSymbol)
  }

  case class Text(value: String) extends TextRepr {
    def render(options: RenderOptions): String = {
      import options._
      spaceSymbol * level + value
    }
  }

  def lines(elems: Seq[TextRepr]): TextRepr = Lines(elems)
  def inlined(s: String): TextRepr = Text(s)
}

