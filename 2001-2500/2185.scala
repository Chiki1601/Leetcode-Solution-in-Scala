  object Solution {
    def prefixCount(words: Array[String], pref: String): Int = {
      words.count(_.startsWith(pref))
    }
  }
