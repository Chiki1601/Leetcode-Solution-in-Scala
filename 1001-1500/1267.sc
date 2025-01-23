object Solution {
  def countServers(grid: Array[Array[Int]]): Int = {
    val rows = grid.length
    val cols = grid(0).length

    // Compute the sum of servers in each row and column
    val rowSums = grid.map(_.sum)
    val colSums = (0 until cols).map(j => (0 until rows).map(i => grid(i)(j)).sum)

    // Count servers that can communicate
    grid.zipWithIndex.flatMap { case (row, i) =>
      row.zipWithIndex.collect {
        case (cell, j) if cell == 1 && (rowSums(i) > 1 || colSums(j) > 1) => 1
      }
    }.sum
  }
}
