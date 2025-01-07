object Solution {
    def stringMatching(words: Array[String]): List[String] = {
        words.filter(word => words.exists(other => other != word && other.contains(word))).toList
    }
}
