object Solution {
  def addSpaces(s: String, spaces: Array[Int]): String = {
    (0 +: spaces)
      .zip(spaces :+ s.length)
      .map { case (start, end) => s.substring(start, end) }
      .mkString(" ")
  }
}
