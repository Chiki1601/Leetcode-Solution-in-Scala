  object Solution {
    def gridGame(grid: Array[Array[Int]]): Long = {
      val topSum = grid.head.map(_.toLong).sum - grid.head.head
      val botSum = 0L
      (1 until grid.head.length)
        .foldLeft((topSum, topSum, botSum))({
          case ((best, topSum, botSum), i) =>
            val ts = topSum - grid(0)(i)
            val bs = botSum + grid(1)(i - 1)
            (Math.min(best, Math.max(ts, bs)), ts, bs)
        })
        ._1
    }
  }
