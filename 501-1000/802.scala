object Solution {
  def eventualSafeNodes(graph: Array[Array[Int]]): List[Int] = {
    val n = graph.length

    // Array to store the state of each node
    // 0 = unvisited, 1 = visiting, 2 = safe
    val state = Array.fill(n)(0)

    // DFS function to check if a node is safe
    def isSafe(node: Int): Boolean = {
      state(node) match {
        case 1 => false // Node is being visited, cycle detected
        case 2 => true  // Node is already safe
        case _ =>       // Node is unvisited
          state(node) = 1 // Mark as visiting
          val safe = graph(node).forall(isSafe)
          state(node) = if (safe) 2 else 0 // Mark as safe or reset to unvisited
          safe
      }
    }

    // Find all safe nodes
    (0 until n).filter(isSafe).toList
  }
}
