package mdparse.parser

import fastparse.all._

import scala.collection.mutable

trait Basic {

  val space = P( " " )
  val ln = StringIn("\r\n", "\n")
  val tab = StringIn("\t")
  val lnOrEnd = P(ln | End)

  val blankLine = P(space.rep ~ ln)

  def WhileNotIn(s: Char*): P0 = CharsWhile(c => !s.contains(c))

  val AnyTextChar = P(!ln ~ AnyChar)

  val WordBreakers = P(ln | tab | space)
  val Word = P(!(ln | tab | space) ~ AnyChar).rep(1)

  val defaultWordBreakers = Set(' ', '\n', '\t', '\r')
  def WordCharsWhile(c: Char*) = {
    val set = defaultWordBreakers ++ c.toSet
    CharsWhile(c => !set.contains(c))
  }

  def wrappedBy(s: String): P[String] = wrappedBy(s, s)

  def wrappedBy(start: String, end: String, min: Int = 1): P[String] = {
    P(start ~ (!end ~ AnyTextChar).rep(min).! ~ end)
  }

  def wrappedBy[A](start: String, end: String, p: P[A]): P[A] = {
    val inner = (!end ~ AnyTextChar).rep(1).!.map(s => {
      p.parse(s) match {
        case Parsed.Success(v, _) => v
        case f:Parsed.Failure => throw new RuntimeException(f.msg)
      }
    })
    P(start ~ inner ~ end)
  }

  def charsToBuilders(elems: Seq[Any]): Seq[Any] = {
    val init = new mutable.ArrayBuffer[Any](elems.size)
    elems.foldLeft(init) {
      case (x, c: String) => x.lastOption match {
        case Some(b: StringBuilder) =>
          b.append(c)
          x
        case _ =>
          val b = new StringBuilder
          b.append(c)
          x :+ b
      }
      case (x, ()) => x
      case (x, ele) =>
        x :+ ele
    }
  }

}

object Basic extends Basic
