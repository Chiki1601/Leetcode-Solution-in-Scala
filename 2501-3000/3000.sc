object Solution {
  def areaOfMaxDiagonal(dimensions: Array[Array[Int]]): Int =
    dimensions.iterator.map(a => (a.map(x => x*x).sum, a.product)).max._2
}
