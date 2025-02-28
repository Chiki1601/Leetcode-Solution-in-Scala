  object Solution {
    def shortestCommonSupersequence(str1: String, str2: String): String = {
      val dp = Array.ofDim[(Int, Int)](str1.length + 1, str2.length + 1)
      extension (p: (Int, Int)) {
        def cost: Int = dp(p._1)(p._2)._1
        def dir: Int = dp(p._1)(p._2)._2
        def left: (Int, Int) = (p._1 - 1, p._2)
        def down: (Int, Int) = (p._1, p._2 - 1)
        def diag: (Int, Int) = left.down
        def setCost(tup: (Int, Int)): Unit = dp(p._1)(p._2) = tup
      }
      (0 to str1.length).foreach(i1 => (i1, 0).setCost(i1, 1))
      (0 to str2.length).foreach(i2 => (0, i2).setCost(i2, 2))
      for (
        i1 <- 1 to str1.length;
        i2 <- 1 to str2.length;
        pos = (i1, i2)
      ) {
        pos.setCost({
          if (str1(i1 - 1) == str2(i2 - 1))
            (1 + pos.diag.cost, 3)
          else {
            if (pos.left.cost < pos.down.cost) {
              (1 + pos.left.cost, 1)
            } else
              (1 + pos.down.cost, 2)
          }
        })
      }
      var pos = (str1.length(), str2.length())
      var ans = new StringBuilder()
      while (pos != (0, 0)) {
        pos = if (pos.dir == 3) {
          ans += str1(pos._1 - 1)
          pos.diag
        } else if (pos.dir == 2) {
          ans += str2(pos._2 - 1)
          pos.down
        } else {
          ans += str1(pos._1 - 1)
          pos.left
        }
      }
      ans.toString.reverse
    }
  }
