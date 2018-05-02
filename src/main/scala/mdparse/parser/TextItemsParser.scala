package mdparse.parser

import fastparse.all._
import mdparse.MdItem.Image
import mdparse.MdItem._
import mdparse.SpanItem

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait TextItemsParser extends Basic {

  val link: P[Link] = {
    val short = wrappedBy("<", ">").map(l => Link(l, l, None))
    val long = {
      val title = wrappedBy("[", "]", 0)
      val dest = wrappedBy("(", ")")
      P(title ~ dest).map({case (t, d) => Link(t, d, None)})
    }
    P(short | long)
  }


  val image: P[Image] = {
    val title = P("\"" ~ (!"\"" ~ AnyTextChar).rep(1).! ~ "\"")
    val dest = P("<".? ~ (!(">" | ")" | WordBreakers) ~ AnyChar).rep(1).! ~ ">".?)
    val end = P("(" ~ dest ~ space.rep(0) ~ title.? ~ space.rep(0) ~ ")")

    val alt = wrappedBy("![", "]", 0)
    P(alt ~ end).map({case (alt, (dest, title)) => Image(dest, alt, title)})
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

  val handle: P[Common] = {
    val inside = P(image | link | italic | strong | code | !"]" ~ AnyTextChar.!).rep(0).map(x => foldChars(x))
    P("!".!.? ~ P("[" ~ inside ~ "]").rep(min = 1, sep = P(""))).map({case (isImage, data) => {
      val t = if (isImage.isDefined) MaybeImage else MaybeLink
      val handle = ResolveHandles(data, t)
      Common(handle)
    }})
  }

  val code = wrappedBy("`").map(s => Code(s))

  val text: P[Seq[SpanItem]] = {
    P(image| link | handle | italic | strong | code | AnyTextChar.!).rep(1).map(foldChars)
  }

  //TODO - remove?
  val textTrimmed: P[Seq[SpanItem]] = {
    P(" ".rep ~ image | link | handle | italic | strong | code | AnyTextChar.!).rep(1)
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

  private def foldChars(elems: Seq[Any]): mutable.Buffer[SpanItem] = {

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
      case x: SpanItem => x
      case x => throw new IllegalArgumentException(s"Invalid argument $x")
    })
  }
}

object TextItemsParser extends TextItemsParser
