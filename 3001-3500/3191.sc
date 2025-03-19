  object Solution {
    def minOperations(nums: Array[Int]): Int = {
      @scala.annotation.tailrec
      def rec(start: Int, total: Int): Int = {
        if (start + 2 == nums.size) {
          if (nums(start) == 1 && nums(start + 1) == 1)
            total
          else -1
        } else if (nums(start) == 0) {
          nums(start + 1) = 1 - nums(start + 1)
          nums(start + 2) = 1 - nums(start + 2)
          rec(start + 1, total + 1)
        } else {
          rec(start + 1, total)
        }
      }
      rec(0, 0)
    }
  }
