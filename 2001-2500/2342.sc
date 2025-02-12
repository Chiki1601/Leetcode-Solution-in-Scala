object Solution {
   def maximumSum(nums: Array[Int]): Int = {
  val map = scala.collection.mutable.Map.empty[Int, Int]
  var max = - 1
  nums.foreach { i =>
    val key = i.toString().map(c => (c - '0').toInt).sum
    val prev = map.get(key)
    prev.foreach(j => max = math.max(max, i + j))
    map(key) = prev.fold(i)(math.max(_, i))
  }
  max
}
}
