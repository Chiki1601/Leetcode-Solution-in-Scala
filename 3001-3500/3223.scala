  object Solution {
    def minimumLength(s: String): Int = {
      s.groupBy(identity)
        .map({
          case (k, v) => {
            val s = v.size
            if (s <= 2)
              s
            else {
              if (s % 2 == 1)
                1
              else
                2
            }
          }
        })
        .sum
    }
  }
