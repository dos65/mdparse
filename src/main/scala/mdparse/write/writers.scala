package mdparse.write

import mdparse.md.{Markdown, MdItem}

object writers {

  implicit class PrettyOps(product: Product) {
    def printDebug: String = DebugPrinter.write(product)
  }

}

object DebugPrinter {

  case class ShiftedBuilder(shift: Int, underlying: StringBuilder) {

    private val start = if ( shift > 0 ) " " * shift  else ""

    def appendLine(s: String*): this.type = {
      underlying.append(start)
      s.foreach(underlying.append)
      underlying.append("\n")
      this
    }

    def appendLines(s: Seq[String]): this.type = {
      s.foreach(s => appendLine(s))
      this
    }

    def shift(i: Int): ShiftedBuilder = ShiftedBuilder(shift + i, underlying)

    def build: String = underlying.toString()
  }

  val shiftSize = 4

  def write(md: Product): String = {
    val builder = ShiftedBuilder(0, new StringBuilder)
    write(md, builder)
    builder.build
  }

  private def write(value: Any, builder: ShiftedBuilder): Unit = {
    value match {
      case tr: Traversable[_] =>
        tr.foreach(i => write(i, builder))
      case p: Product => writeP(p, builder)
      case s: String => builder.appendLine("\"", s, "\"")
      case x => builder.appendLine(x.toString)
    }
  }

  private def writeP(p: Product, builder: ShiftedBuilder): Unit = {
    builder.appendLine(p.getClass.getSimpleName, "(")
    val nested = builder.shift(shiftSize)
    val values = p.productIterator
    if (p.productArity == 0) {
      builder.appendLine(p.getClass.getSimpleName)
    } else if (p.productArity == 1) {
      p.getClass.getDeclaredFields.foreach(f => {
        write(values.next(), nested)
      })
    } else {
      val inside = nested.shift(shiftSize)
      p.getClass.getDeclaredFields.foreach(f => {
        nested.appendLine(f.getName, ":")
        write(values.next(), inside)
      })
    }

    builder.appendLine(")")
  }

}
