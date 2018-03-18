package mdparse

import fastparse.all._
import mdparse.parser.MdParser

object Main extends App {

  val in = args(0)

  MdParser.markdown.parse(in) match {
    case Parsed.Success(data, _) =>
      println(data)
    case f: Parsed.Failure =>
      println("Error occurred during parsing:" + f.msg)
      sys.exit(1)
  }
}
