package mdparse

import fastparse.all._
import mdparse.parser.MdParser

object Main extends App {

  val in = args(0)

  MdParser.parse(in) match {
    case Right(data) =>
      println(data)
    case Left(err) =>
      println(s"Error occurred during parsing:$err")
      sys.exit(1)
  }
}
