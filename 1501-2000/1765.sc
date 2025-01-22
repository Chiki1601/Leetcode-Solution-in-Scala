object Solution {
  def highestPeak(isWater: Array[Array[Int]]): Array[Array[Int]] = {
    val directions = List((0, 1), (1, 0), (0, -1), (-1, 0))
    val m = isWater.length
    val n = isWater(0).length

    // Initialize the result matrix and the queue
    val height = Array.fill(m, n)(-1)
    val queue = scala.collection.mutable.Queue[(Int, Int)]()

    // Enqueue all water cells and set their height to 0
    for {
      i <- 0 until m
      j <- 0 until n
      if isWater(i)(j) == 1
    } {
      height(i)(j) = 0
      queue.enqueue((i, j))
    }

    // BFS to assign heights
    while (queue.nonEmpty) {
      val (x, y) = queue.dequeue()
      val currentHeight = height(x)(y)

      // Process all 4 neighboring cells
      for ((dx, dy) <- directions) {
        val nx = x + dx
        val ny = y + dy
        if (nx >= 0 && nx < m && ny >= 0 && ny < n && height(nx)(ny) == -1) {
          height(nx)(ny) = currentHeight + 1
          queue.enqueue((nx, ny))
        }
      }
    }

    height
  }
}
