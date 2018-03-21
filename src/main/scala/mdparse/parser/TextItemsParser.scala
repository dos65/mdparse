package mdparse.parser

import fastparse.all._
import mdparse.md._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait TextItemsParser extends Basic {

  val link = {
    val short = wrappedBy("<", ">").map(l => Link(l))
    val long = {
      val title = wrappedBy("[", "]")
      val dest = wrappedBy("(", ")")
      P(title ~ dest).map({case (t, d) => Link(t, d)})
    }
    P(short | long)
  }

  // split thta bullshit into to different object
  // ![foo] - should be defenition
  // [foo]:/url resolution
  val image: P[Image] = {
    val title = P("\"" ~ (!"\"" ~ AnyTextChar).rep(1).! ~ "\"")
    val dest = P("<".? ~ (!(">" | ")" | WordBreakers) ~ AnyChar).rep(1).! ~ ">".?)

    val defaultEnd: P[(String, Option[String])] =
      P("(" ~ dest ~ space.rep(0) ~ title.? ~ space.rep(0) ~ ")")

    def colonEnd(alt: String): P[(String, Option[String])] = {
      P("\n".rep(max = 2)  ~ "[" ~ alt ~ "]: " ~ Word.! ~ (space ~ "\"" ~ (!("\"" | ln | tab) ~ AnyChar).rep(1).! ~ "\"").?)
    }

    def toRaw(t: TextItem): String = t match {
      case Common(s) => s
      case Strong(elems) => elems.map(toRaw).mkString("")
      case Italic(elems) => elems.map(toRaw).mkString("")
      case Code(s) => s
      case Image(_, alt, _) => alt
      case Link(text, _) => text
    }

    def toDefault(t: TextItem): String = t match {
      case Common(s) => s
      case Strong(elems) => "_" + elems.map(toDefault).mkString("") + "_"
      case Italic(elems) => "*" + elems.map(toDefault).mkString("") + "*"
      case Code(s) => s
      case Image(_, alt, _) => alt
      case Link(text, _) => text
    }

    val altP = {
      val inside = P(image | link | italic | strong | code | !"]" ~ AnyTextChar.!).rep(0).map(x => foldChars(x))
      P("[" ~ inside ~ "]")
    }

    P("!" ~ altP.rep(min = 1, sep = P(""))).flatMap(alts => {

      val expectedAlts = alts.flatMap(alt => {
        val default = alt.map(toDefault).mkString("")
        val raw = alt.map(toRaw).mkString("")
        Set(default, raw, raw.toUpperCase, raw.toLowerCase).toList
      })
      println(expectedAlts)

      val expectedEnds = expectedAlts.map(colonEnd).reduceLeft(_ | _)

      (defaultEnd | expectedEnds).map({case (dest, title) => {
        val raw = alts.head.map(toRaw).mkString
        Image(dest, raw, title)
      }})
    })
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

  val code = wrappedBy("`").map(s => Code(s))

  val text: P[Seq[TextItem]] = {
    P(image | link | italic | strong | code | AnyTextChar.!).rep(1).map(foldChars)
  }

  val textTrimmed: P[Seq[TextItem]] = {
    P(" ".rep ~ image | link | italic | strong | code | AnyTextChar.!).rep(1)
      .map(raw => {
        val items = foldChars(raw)
        if (items.size == 1) {
          items(items.size - 1) = items.last match {
            case Common(s) => Common(s.replaceAll("^\\s+", "").replaceAll("\\s+$", ""))
            case x => x
          }

        } else {
          items(0) = items.head match {
            case Common(s) => Common(s.replaceAll("^\\s+", ""))
            case x => x
          }
          items(items.size - 1) = items.last match {
            case Common(s) => Common(s.replaceAll("\\s+$", ""))
            case x => x
          }
        }
        items
      })
  }

  private def foldChars(elems: Seq[Any]): mutable.Buffer[TextItem] = {

    def toBuilders(seq: mutable.Buffer[Any]): mutable.Buffer[Any] = {
      val init = new mutable.ArrayBuffer[Any](seq.size)
      seq.foldLeft(init) {
        case (x, c: String) => x.lastOption match {
          case Some(b: StringBuilder) =>
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
    }

    toBuilders(elems.asInstanceOf[ArrayBuffer[Any]]).map({
      case builder: StringBuilder =>
        val s = builder.mkString
        Common(s)
      case x: TextItem => x
      case x => throw new IllegalArgumentException(s"Invalid argument $x")
    })
  }
}

object TextItemsParser extends TextItemsParser
