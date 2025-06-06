object Solution {
  def lengthOfLIS(nums: Array[Int]): Int = {
    val dp = Array.fill(nums.length)(1)

    for {
      l <- 1 until nums.length
      r <- 0 until l 
      if nums(l) > nums(r)
    } dp(l) = Math.max(dp(l), dp(r) + 1)

    dp.max
  }
}
