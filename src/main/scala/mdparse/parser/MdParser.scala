package mdparse.parser

import fastparse.all._
import mdparse._
import mdparse.MdItem._

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
    type Inner = Either[MdList, Seq[SpanItem]]

    val prefixes = Seq(
      ListPrefix(P("*"), UnorderedList.apply),
      ListPrefix(P("-"), UnorderedList.apply),
      ListPrefix(P("+"), UnorderedList.apply),
      ListPrefix(P(CharIn('0' to '9').rep(1) ~ "."), OrderedList.apply)
    )

    def listItem(
      head: Seq[SpanItem],
      nextPref: ListPrefix,
      level: Int): P[ListItem] = {

      val maybePrev = for {
        i <- 0 to level
        prefix <- prefixes
      } yield prefix.parser(i)
      val catchItems = maybePrev.reduceLeft(_ | _)

      val stop = catchItems | thBreak | blankLine.rep(2)

      val inner: P[Inner] = mkLists(level + 1).map(l => Left(l))

      val line: P[Inner] = P(TextItemsParser.textTrimmed ~/ lnOrEnd).map(l => Right(l))

      P(!stop ~ (inner | line)).rep(0).map(body => {
        val cleaned = body.foldLeft(List.empty[MdItem]) {
          case (acc, Left(list)) => acc :+ list
          case (acc, Right(items)) => acc ++ items
        }
        ListItem(head ++ cleaned)
      })
    }

    def mkList(prefix: ListPrefix, level: Int): P[MdList] = {
      val prefixP = prefix.parser(level)
      val headParser = P(prefixP ~ space ~ TextItemsParser.textTrimmed ~/ lnOrEnd)
      headParser.flatMap(head => listItem(head, prefix, level))
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

    val forTab = P(tab ~ (CharsWhile(c => c != '\n' && c != '\r') | !ln ~ AnyChar).!.rep(1) ~ (ln | End)).rep(1)
      .map(lines => FencedCode(None, lines.flatten.mkString("\n")))

    forTab | forSymbol('`') | forSymbol('~')
  }

  val resolveReference: P[Reference] = {
    P(wrappedBy("[", "]") ~ ":" ~ space.rep(0) ~ Word.! ~ (space.rep(0) ~ "\"" ~ (!"\"" ~ AnyTextChar).rep(1).! ~ "\"").? ~ (ln | End))
      .map({case (ref, dest, title) => Reference(ref, dest, title)})
  }

  val rawMarkdown: P[RawMarkdown] = {
    type Block = Either[Reference, BlockItem]
    val items: P[Block] =
      fencedCode.map(f => Right(f)) |
      header.map(h => Right(h)) |
      thBreak.map(b => Right(b))|
      resolveReference.map(r => Left(r)) |
      HtmlParser.html.map(h => Right(h)) |
      list.map(l => Right(l)) |
      paragraph.map(p => Right(p))

    P(items.rep ~ End).map(all => {
      val (blocks, refs) = all.foldLeft((Vector.empty[BlockItem], Vector.empty[Reference])) {
        case ((blocks, refs), Right(block)) => (blocks :+ block, refs)
        case ((blocks, refs), Left(ref)) => (blocks, refs :+ ref)
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
