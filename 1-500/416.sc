  object Solution {
    var doLog = false
    def println(a: Any): Unit =
      if (doLog)
        Console.out.println(a)
    def canPartition(nums: Array[Int]): Boolean = {
      val total = nums.sum
      val n = nums.length
      val dp = Array.ofDim[Int](total / 2 + 1, n)
      (0 until n).foreach(ind => dp(0)(ind) = 1)
      // pprintln(dp)
      // @logged
      def cansubsetsum(sum: Int, from: Int): Boolean = {
        sum >= 0 && from < n && {
          if (dp(sum)(from) == 0) {
            dp(sum)(from) =
              if (
                cansubsetsum(sum, from + 1) ||
                cansubsetsum(sum - nums(from), from + 1)
              ) 1
              else 2
          }
          dp(sum)(from) == 1
        }
      }
      total % 2 == 0 && {
        cansubsetsum(total / 2, 0)
      }
    }
  }
