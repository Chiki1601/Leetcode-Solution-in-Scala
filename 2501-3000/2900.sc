object Solution {
    def getLongestSubsequence(words: Array[String], groups: Array[Int]): List[String] = {
        words.zip(groups).foldLeft((List[String](), -1)) {
            case ((list, prev), (word, group)) =>
                if (prev != group) (list :+ word, group)
                else (list, group)
        }._1
    }
}
