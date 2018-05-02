package mdparse.parser

import fastparse.all._
import mdparse._
import mdparse.MdItem._

import scala.annotation.switch

trait MdParser extends Basic {

  val header = {
    val sharps = P ( "#".rep(min = 1, max = 6).! ).map(_.length)

    val readLine = P(CharsWhile(c => c != '\n' && c != '\r').!)
    val text = P(!ln ~ readLine.! ~/ lnOrEnd).map(_.trim)

    //TODO
    //P( sharps ~ space ~/ text ).map({case (level, text) => Header(level, text)})
    P( sharps ~ space ~/ text ).map({case (level, text) => Header(level, Seq(Common(text)))})
  }

  val thBreak = {
    val spaces = P( space.rep(max = 3) )

    val stars = P( "*".rep(min = 3) )
    val underscores = P( "_".rep(min = 3) )
    val dashes = P( "-".rep(min = 3) )

    val symbolic = P((dashes | stars | underscores) ~ space.rep ~ lnOrEnd)
    P(spaces.? ~ symbolic).map(_ => ThBreak)
  }

  val paragraph = {
    P(TextItemsParser.text ~ lnOrEnd).rep(1).map(items => Paragraph(items.flatten))
  }

  case class ListPrefix(sym: P0, f: Seq[ListItem] => MdList) {
    def parser(level: Int): P0 = if (level > 0) {
      val spaces = P(" ".!.rep(min = level * 2 , max = level * 4)).filter(_.length % 2 == 0).map(_ => ())
      val tabs = P("\t".rep(exactly = level))
      (spaces | tabs) ~ sym
    } else {
      sym
    }
  }


  val list = {
    type Line = Seq[SpanItem]
    type Inner = Either[MdList, Line]

    val prefixes = Seq(
      ListPrefix(P("*"), UnorderedList.apply),
      ListPrefix(P("-"), UnorderedList.apply),
      ListPrefix(P("+"), UnorderedList.apply),
      ListPrefix(P(CharIn('0' to '9').rep(1) ~ "."), OrderedList.apply)
    )

    def listItem(
      head: Seq[SpanItem],
      level: Int): P[ListItem] = {

      val maybePrev = for {
        i <- 0 to level
        prefix <- prefixes
      } yield prefix.parser(i)
      val catchItems = maybePrev.reduceLeft(_ | _)

      val stop = catchItems | thBreak | blankLine.rep(2)

      val inner: P[Inner] = mkLists(level + 1).map(l => Left(l))

      val empty: P[Inner] = blankLine.map(_ => Right(Seq.empty))
      val line: P[Inner] = P(TextItemsParser.textTrimmed ~/ lnOrEnd).map(l => Right(l))

      P(!stop ~ (inner | line | empty)).rep(0).map(body => {

        val (lines, inners) = body.foldLeft((Vector.apply(head), Vector.empty[MdList])) {
          case ((lines, inners), Left(list)) => (lines, inners :+ list)
          case ((lines, inners), Right(line)) => (lines :+ line, inners)
        }

        @switch
        val items = lines.length match {
          case 0 => inners
          case 1 => lines.flatten ++ inners
          case _ => lines.filter(_.nonEmpty).map(Paragraph) ++ inners
        }

        ListItem(items)
      })
    }

    def mkList(prefix: ListPrefix, level: Int): P[MdList] = {
      val prefixP = prefix.parser(level)
      // TODO?
      val prepre = if (level == 0 ) P(space.rep()) else P("")
      val headParser = P(prepre ~ prefixP ~ space.rep(min = 1).! ~ TextItemsParser.textTrimmed ~/ lnOrEnd)
      headParser.flatMap({ case(spaces, head) => listItem(head, level) })
        .rep(1)
        .map(items => prefix.f(items))
    }

    def mkLists(level: Int): P[MdList] = prefixes.map(p => mkList(p, level)).reduceLeft(_ | _)

    mkLists(0)
  }

  val fencedCode: P[FencedCode] = {
    def forSymbol(ch: Char): P[FencedCode] = {
      def wrapper(min: Int) = ch.toString.rep(min)

      P(wrapper(3).!).flatMap(s => {
        val lastMin = s.length
        val end = wrapper(lastMin)
        P(Word.!.? ~ ln ~ (CharsWhile(c => c != ch && c != '\n' && c != '\r') | !(ln ~ end) ~ AnyChar).!.rep(1) ~ ln ~ wrapper(lastMin))
          .map({case (lang, code) => FencedCode(lang, code.mkString(""))})
      })
    }
    val toLineEnd = P(CharsWhile(c => c != '\n' && c != '\r') | !ln ~ AnyChar).!.rep(1).map(_.mkString(""))

    val forTab = P(space.rep(0) ~ tab ~ toLineEnd ~ (ln | End)).rep(1).map(lines => FencedCode(None, lines.mkString("\n")))

    val forSpace = P(space.rep(exactly = 4) ~ toLineEnd ~ (ln | End)).rep(1)
      .map(lines => FencedCode(None, lines.flatten.mkString("\n")))

    forTab | forSpace | forSymbol('`') | forSymbol('~')
  }

  val resolveReference: P[Reference] = {
    P(wrappedBy("[", "]") ~ ":" ~ space.rep(0) ~ Word.! ~ (space.rep(0) ~ "\"" ~ (!"\"" ~ AnyTextChar).rep(1).! ~ "\"").? ~ (ln | End))
      .map({case (ref, dest, title) => Reference(ref, dest, title)})
  }

  sealed trait ParsedItem
  case object BlankLine extends ParsedItem
  final case class Ref(r: Reference) extends ParsedItem
  final case class Item(i: BlockItem) extends ParsedItem

  val rawMarkdown: P[RawMarkdown] = {
    val items: P[ParsedItem] =
      fencedCode.map(f => Item(f)) |
      header.map(h => Item(h)) |
      thBreak.map(b => Item(b))|
      resolveReference.map(r => Ref(r)) |
      HtmlParser.html.map(h => Item(h)) |
      list.map(l => Item(l)) |
      paragraph.map(p => Item(p)) |
      blankLine.map(_ => BlankLine)


    P(items.rep ~ End).map(all => {
      val (blocks, refs) = all.foldLeft((Vector.empty[BlockItem], Vector.empty[Reference])) {
        case ((blocks, refs), Item(block)) => (blocks :+ block, refs)
        case ((blocks, refs), Ref(ref)) => (blocks, refs :+ ref)
        case ((blocks, refs), BlankLine) => (blocks, refs)
      }
      RawMarkdown(blocks, refs)
    })
  }
}

object MdParser extends MdParser {

  def parse(s: String): Either[String, Markdown] = {
    rawMarkdown.parse(s) match {
      case Parsed.Success(r, _) => Right(r.resolve())
      case f: Parsed.Failure => Left(f.msg)
    }
  }

}
