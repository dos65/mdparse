package mdparse

import fastparse.all._
import mdparse.md._

trait TextItemsParser extends Basic {

  val AnyTextChar = P(!(ln | tab) ~ AnyChar)

  private def wrappedBy(s: String): P[String] = wrappedBy(s, s)

  private def wrappedBy(start: String, end: String): P[String] = {
    P(start ~ (!end ~ AnyTextChar).rep(1).! ~ end)
  }

  val link = {
    val short = wrappedBy("<", ">").map(l => Link(l))
    val long = {
      val title = wrappedBy("[", "]")
      val dest = wrappedBy("(", ")")
      P(title ~ dest).map({case (t, d) => Link(t, d)})
    }
    P(short | long)
  }


  val strong = {
    val italic = {
      val inner = P(!"*" ~ (link | AnyTextChar.!)).rep(1).map(foldChars)
      P("*" ~ inner ~ "*").map(s => Italic(s))
    }

    def mkP(sym: String) = {
      val inner = P(!sym ~ (italic | link | AnyTextChar.!)).rep(1)
      P(sym ~ inner ~ sym)
    }

    (mkP("**") | mkP("__")).map(raw => {
      val items = foldChars(raw)
      Strong(items)
    })
  }

  val italic = {
    val strong = (wrappedBy("**") | wrappedBy("__")).map(s => Strong(List(Common(s))))

    val inner = P(!"*" ~ (strong | link | AnyTextChar.!)).rep(1)

    P("*" ~ inner ~ "*").map(raw => {
      val items = foldChars(raw)
      Italic(items)
    })
  }

  val text = {
    P(link | italic | strong | AnyTextChar.!).rep(1)
      .map(raw => {
        val items = foldChars(raw)
        Text(items)
      })
  }

  private def foldChars(elems: Seq[Any]): Seq[TextItem] = {

    def toBuilders(seq: Seq[Any]): Seq[Any] = seq.foldLeft(List.empty[Any]) {
      case (x, c: String) => x.lastOption match {
        case Some(b :StringBuilder) =>
          b.append(c)
          x
        case _ =>
          val b = new StringBuilder
          b.append(c)
          x :+ b
      }
      case (x, ele) =>
        x :+ ele
    }

    toBuilders(elems).map({
      case builder: StringBuilder =>
        val s = builder.mkString
        Common(s)
      case x: TextItem => x
      case x => throw new IllegalArgumentException(s"Invalid argument $x")
    })
  }
}

object TextItemsParser extends TextItemsParser
