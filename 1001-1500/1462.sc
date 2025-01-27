object Solution {
  def checkIfPrerequisite(numCourses: Int, prerequisites: Array[Array[Int]], queries: Array[Array[Int]]): List[Boolean] = {
    val dist = Array.ofDim[Int](numCourses, numCourses)

    for(Array(from, to) <- prerequisites)
      dist(from)(to) = 1

    for(k <- 0 until numCourses)
      for(i <- 0 until numCourses)
        if(dist(i)(k) == 1)
          for(j <- 0 until numCourses)
            if(dist(k)(j) == 1)
              dist(i)(j) = 1

    queries
      .iterator
      .map { case Array(from, to) => dist(from)(to) == 1 }
      .toList
  }
}
