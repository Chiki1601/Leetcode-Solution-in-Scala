object Solution {
  def findMaxFish(grid: Array[Array[Int]]): Int = {
    def dfs(r: Int, c: Int, g: Array[Array[Int]]):Int = {
      if (r < 0 || c < 0|| r == g.size || c == g(0).size || g(r)(c) == 0) 0
      else {
        val t = g(r)(c)
        g(r)(c) = 0
        t + dfs(r + 1, c, g) + dfs(r, c + 1, g) + dfs(r - 1, c, g) + dfs(r, c - 1, g)
      }
    }
    (for{r <- 0 until grid.length
        c <- 0 until grid(0).length
    }yield dfs(r,c, grid)).max
  }
}
