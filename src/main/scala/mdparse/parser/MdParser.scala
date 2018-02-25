package mdparse.parser

import fastparse.all._
import mdparse.Markdown
import mdparse.md._

trait MdParser extends Basic {

  val header = {
    val sharps = P ( "#".rep(min = 1, max = 6).! ).map(_.length)

    val readLine = P(CharsWhile(c => c != '\n' && c != '\r').!)
    val text = P(!ln ~ readLine.! ~/ lnOrEnd).map(_.trim)

    P( sharps ~ space ~/ text ).map({case (level, text) => Header(level, text)})
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
    P(TextItemsParser.text ~ lnOrEnd).rep(1).map(items => Paragraph(items))
  }

  val list = {

    case class ListPrefix[T](sym: P0, f: Seq[ListItem] => T) {

      def parser(level: Int): P0 = {
        if (level > 0) {
          val spaces = space.rep(exactly = level * 4)
          spaces ~ sym
        } else {
          sym
        }
      }

    }

    val prefixes = Seq(
      ListPrefix(P("*"), UnorderedList.apply),
      ListPrefix(P("-"), UnorderedList.apply),
      ListPrefix(P("+"), UnorderedList.apply),
      ListPrefix(P(CharIn('0' to '9').rep(1) ~ "."), OrderedList.apply)
    )

    def listItem(
      head: Text,
      prefix: ListPrefix[_],
      level: Int): P[ListItem] = {

      val maybeNexts = for ( i <- 0 to level + 1 ) yield prefix.parser(i)
      val catchItems = maybeNexts.reduceLeft(_ | _)

      val notNextItem = !(catchItems | thBreak )
      val content = P(notNextItem ~ TextItemsParser.textTrimmed ~ lnOrEnd)

      val inner = mkLists(level + 1)
      (content.rep(0) ~ inner.rep(0)).map({case (content: Seq[Text],  inner: Seq[MdList]) => ListItem(head +: (content ++ inner)) })
    }

    def mkList[T](prefix: ListPrefix[T], level: Int): P[T] = {
      val headParser = P(prefix.parser(level) ~ space ~ TextItemsParser.textTrimmed ~/ lnOrEnd)
      headParser.flatMap(head => listItem(head, prefix, level))
        .rep(1)
        .map(items => prefix.f(items))
    }

    def mkLists(level: Int): P[MdList] = prefixes.map(p => mkList(p, level)).reduceLeft(_ | _)

    mkLists(0)
  }

  val markdown: P[Markdown] = {
    val items = header | thBreak | blankLine | list | paragraph
    P(items.rep ~ End).map(all => {
      val mdItems = all.collect({case b: MdItem => b})
      Markdown(mdItems)
    })
  }

}

object MdParser extends MdParser
