object Solution {
  def findMissingAndRepeatedValues(grid: Array[Array[Int]]): Array[Int] = {
    val set = grid.flatten.groupMapReduce(identity)(_ => 1)(_ + _).withDefaultValue(0)
    Iterator.range(1,(grid.length)*(grid.head.length)+1)
      .map(x => x -> set(x)).filter(_._2 != 1)
      .toArray.sortBy(- _._2).map(_._1)
  }
}
