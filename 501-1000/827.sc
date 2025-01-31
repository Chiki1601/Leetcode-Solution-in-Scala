object Solution {

  def largestIsland(grid: Array[Array[Int]]): Int = {
    // directions to check for neighbors
    val deltas = List((-1, 0), (1, 0), (0, -1), (0, 1))

    // all possible (x, y) combinations to check when iterating over the grid
    val indices = for {
      i <- grid.indices
      j <- grid.indices
    } yield (i, j)

    // helper function that gets the (x, y) indices that neighbor a cell
    // and satisfy a given condition
    def validNeighbors(x: Int, y: Int, cond: Int => Boolean = _ => true): List[(Int, Int)] =
      deltas
        .map { case (dx, dy) => (x + dx, y + dy) }
        .filter { case (nx, ny) =>
          nx >= 0 &&
          nx < grid.length &&
          ny >= 0 &&
          ny < grid.length &&
          cond(grid(nx)(ny))
        }

    // breadth-first search to find an island touching
    // a given cell and mark it in the grid with an identifier
    def bfs(ident: Int, x: Int, y: Int): Int = {
      var size = 0
      val q = collection.mutable.Queue[(Int, Int)]((x, y))
      while (q.nonEmpty) {
        val (x, y) = q.dequeue()
        val num = grid(x)(y)

        if (grid(x)(y) == 1) {
          size += 1
          grid(x)(y) = ident
          validNeighbors(x, y, (v) => v == 1).foreach(q.enqueue(_))
        }
      }
      size
    }

    // build a map of island identifiers to the size of each island
    // and keep track of the biggest island built so far
    val (islands, biggest, _) =
      indices.foldLeft((Map[Int, Int](), 0, 2)) { case ((map, biggest, i), (x, y)) =>
        if (grid(x)(y) == 1) {
          val islandSize = bfs(i, x, y)
          (map + (i -> islandSize), biggest max islandSize, i + 1)
        } else (map, biggest, i)
      }

    if (biggest == grid.length * grid.length)
      biggest // the whole thing is already an island, so we can't make a bigger one
    else {
      indices.foldLeft(biggest + 1) { case (bssf, (i, j)) =>
        if (grid(i)(j) != 0) bssf
        else {
          val neighbors = validNeighbors(i, j, (v) => v != 0)
            .map { case (nx, ny) => grid(nx)(ny) }
            .distinct
            
          if (neighbors.length > 1)
            bssf max (neighbors.map(islands(_)).sum + 1)
          else bssf
        }
      }
    }
  }
}
