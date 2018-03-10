package mdparse.parser

object DebugPrinter {

  implicit class PrettyOps(product: Product) {
    def printDebug: String = DebugPrinter.write(product)
  }

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
      case s: String =>
        s.split("\n").foreach(p => builder.appendLine("\"", p, "\""))
      case tr: Traversable[_] =>
        tr.foreach(i => write(i, builder))
      case p: Product => writeP(p, builder)
      case x => builder.appendLine(x.toString)
    }
  }

  private def writeP(p: Product, builder: ShiftedBuilder): Unit = {
    builder.appendLine(p.getClass.getSimpleName, "(")

    val nested = builder.shift(shiftSize)
    val fields = p.getClass.getDeclaredFields.map(_.getName).toList
    val values = p.productIterator.toList
    val elems = fields zip values

    if (elems.isEmpty) {
      builder.appendLine(p.getClass.getSimpleName)
    } else if (elems.size == 1) {
      elems.foreach({case (_, v) => write(v, nested)})
    } else {
      val inside = nested.shift(shiftSize)
      elems.foreach({case (name, v) =>
        nested.appendLine(name, ":")
        write(v, inside)
      })
    }

    builder.appendLine(")")
  }

}
