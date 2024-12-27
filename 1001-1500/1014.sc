object Solution {
  def maxScoreSightseeingPair(values: Array[Int]): Int = {
    var max = 0;
    var score = values(0);
    for (i <- 1 to (values.length - 1)) {
      score -= 1;
      if (score + values(i) > max) max = score + values(i);
      if (values(i) > score) score = values(i);
    }
    return max;
  }
}
