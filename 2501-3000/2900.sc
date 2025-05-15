object Solution {
  def getWordsInLongestSubsequence(n: Int, words: Array[String], groups: Array[Int]): List[String] = {
    var res = List.empty[String]
    (0 until n).foreach(i => if (i == n - 1 || groups(i) != groups(i + 1)) res ::= words(i))
    res.reverse
  }
}
