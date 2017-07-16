package mdparse

import fastparse.all._
import fastparse.core.Logger._
import fastparse.noApi
import fastparse.utils.ReprOps
import mdparse.md._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer


trait MdParser extends Basic{

  val header = {
    val sharps = P ( "#".rep(min = 1, max = 6).! ).map(_.length)

    val readLine = P(CharsWhile(c => c != '\n' && c != '\r').!)
    val text = P(!ln ~ readLine.! ~/ lnOrEnd).map(_.trim)

    P( sharps ~ space ~/ text ).map({case (level, text) => Header(level, text)})
  }

  val break = {
    val spaces = P( space.rep(max = 3) )

    val stars = P( "*".rep(min = 3) )
    val underscores = P( "_".rep(min = 3) )
    val dashes = P( "-".rep(min = 3) )

    val symbolic = P((dashes | stars | underscores) ~ space.rep ~ lnOrEnd)
    P(spaces.? ~ symbolic).map(_ => Break)
  }

  val paragraph = {
    P(TextItemsParser.text ~ lnOrEnd).rep(1).map(items => Paragraph(items))
  }

  val list = {

    def itemStart(prefix: String, level: Int): P0 = {
      val p = if (level > 0) {
        val spaces = space.rep(exactly = level * 4)
        P(spaces ~ prefix)
      } else {
        P(prefix)
      }

      p ~ space
    }

//    def inner(head: md.Text, prefix: String, level: Int): P[ListItem] = {
//      val notNextItem = !(itemStart(prefix, level) | break )
//      val content = P(notNextItem ~ TextItemsParser.text ~ lnOrEnd).rep(1)
//      val inner = mkUnordered(prefix, level + 1)
//      (inner | content).rep(1).?.map(data => {
//        val casted = data.asInstanceOf[Option[Seq[MdItem]]]
//        ListItem(head +: casted.getOrElse(Seq.empty))
//      })
//    }

//    def mkUnordered(prefix: String, level: Int): P[UnorderedList] = {
//      val headParser = if (level > 0) {
//        val spaces = space.rep(exactly = level * 4)
//        P(spaces ~ prefix ~ space ~ TextItemsParser.text ~/ lnOrEnd)
//      } else {
//        P(prefix ~ space ~ TextItemsParser.text ~/ lnOrEnd)
//      }
//      headParser.flatMap(head => inner(head, prefix, level))
//        .rep(1)
//        .map(items => UnorderedList(items))
//    }

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
      head: md.Text,
      prefix: ListPrefix[_],
      level: Int): P[ListItem] = {

      val maybeNexts = for ( i <- 0 to level ) yield prefix.parser(i)
      val catchItems = maybeNexts.reduceLeft(_ | _)

      val notNextItem = !(catchItems | break )
      val content = P(notNextItem ~ TextItemsParser.textTrimmed ~ lnOrEnd)

      val inner = mkLists(level + 1)
      (inner | content).rep(1).?.map(data => {
        ListItem(head +: data.getOrElse(Seq.empty))
      })
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

  val markdown: P[Seq[MdItem]] = P((header | break | blankLine | paragraph ).rep ~ End)
    .map(_.collect({case b: MdItem => b}))

}

object MdParser extends MdParser
