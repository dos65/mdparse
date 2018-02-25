package mdparse

case class RenderOptions(
  level: Int,
  step: Int,
  spaceSymbol: String,
  sepSymbol: String
) {
  def nextLevel: RenderOptions = copy(level = level + 1)
}

object RenderOptions {
  def default = RenderOptions(0, 4, " ", "\n")
  def inlined = RenderOptions(0, 1, " ", " ")
}

