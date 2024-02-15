object Solution {
  def largestPerimeter(nums: Array[Int]): Long = {
    val res = (nums.sorted.scanLeft(0L)(_ + _).drop(1) zip nums.sorted).filter(n => n._1 > n._2 * 2)
    if(res.isEmpty) -1L else res.map(_._1).max
  }
}
