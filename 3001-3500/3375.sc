object Solution {
  def minOperations(nums: Array[Int], k: Int): Int = {
    val (t,d) = (nums.to(Set) - k).partition(_ < k)
    if(t.isEmpty) d.size else -1
  }
}
