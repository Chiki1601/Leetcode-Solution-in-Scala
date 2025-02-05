  object Solution {
    def areAlmostEqual(s1: String, s2: String): Boolean = {
      s1 == s2 || {
        val mismatches = s1
          .zip(s2)
          .collect({
            case (a, b) if a != b => (a, b)
          })
          .toArray
        mismatches match {
          case Array((a, b), (c, d)) if a == d && b == c => true
          case _                                         => false
        }
      }
    }
  }
