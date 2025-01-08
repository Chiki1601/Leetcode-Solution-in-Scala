object Solution {
    def countPrefixSuffixPairs(words: Array[String]): Int = {
        words.indices.map { j =>
            (j + 1).until(words.length).count(l => l match {
                case matches if words(l).startsWith(words(j)) && words(l).endsWith(words(j)) => true
                case _ => false
            })
        }.sum
    }
}
