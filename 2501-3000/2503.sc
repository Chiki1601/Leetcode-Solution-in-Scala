object Solution {
  def maxPoints(grid: Array[Array[Int]], queries: Array[Int]): Array[Int] = {
    import scala.collection.immutable.{TreeSet, TreeMap}
    val (m, n) = (grid.size, grid.head.size)
    def search(front: TreeSet[(Int, Int)], vis: Set[(Int, Int)], res: TreeMap[Int, Int], qs: List[Int]): TreeMap[Int, Int] = qs match {
      case Nil => res
      case q :: remainQ =>
        val (left, right) = front.span { case (i, j) => grid(i)(j) < q }
        val newRes = res + (q -> (left.size + res.getOrElse(q, res.lastOption.map(_._2).getOrElse(0))))
        val newVis = vis ++ left
        val newFront = right ++ left
          .flatMap { case (i, j) => Seq((i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)) }
          .filter { case (i, j) => 0 <= i && i < m && 0 <= j && j < n }.diff(newVis)
        search(newFront, newVis, newRes, if (left.isEmpty) remainQ else qs)
    }
    val initFront = TreeSet((0, 0))(Ordering.by { case (i, j) => (grid(i)(j), i, j) })
    val map = search(initFront, Set.empty, TreeMap.empty, queries.sorted.toList)
    queries.map(map)
  }
}
