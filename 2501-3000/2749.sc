object Solution {
  def makeTheIntegerZero(num1: Int, num2: Int): Int = {
    def countBits(num: Long): Int = num.toBinaryString.count(_ == '1')
    if (num1 < num2) {
      -1
    } else {
      (0 to 100).find { steps =>
        val diff = num1 - 1L * num2 * steps
        val bits = countBits(diff)
        bits <= steps && steps <= diff
      }.getOrElse(-1)
    }
  }
}
