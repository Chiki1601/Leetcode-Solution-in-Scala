import scala.math._

object Solution {
  def kthCharacter(k: Long, operations: Array[Int]): Char = {
    var i = k
    var transforms = 0

    while (i > 1) {
      val n = floor(log((i - 1).toDouble) / log(2)).toInt
      i -= 1L << n
      if (operations(n) == 1) transforms += 1
    }

    ('a' + (transforms % 26)).toChar
  }
}
