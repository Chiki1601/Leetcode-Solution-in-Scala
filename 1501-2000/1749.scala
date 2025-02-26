  object Solution {
    def maxAbsoluteSum(nums: Array[Int]): Int = {
      var ans = 0
      var pMin = 0
      var pMax = 0
      var i = 0
      while (i < nums.length) {
        val num = nums(i)
        pMax = Math.max(pMax + num, num)
        pMin = Math.min(pMin + num, num)
        ans = Math.max(ans, Math.max(pMax, -pMin))
        i += 1
      }
      ans
    }
  }
