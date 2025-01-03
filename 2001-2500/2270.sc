object Solution {
  def waysToSplitArray(nums: Array[Int]): Int = {
    val left = nums.scanLeft(0L)(_ + _).drop(1)
    val right = nums.scanRight(0L)(_ + _).dropRight(1)
    (left zip right.drop(1)).count(n => n._1 >= n._2)
  }
}
