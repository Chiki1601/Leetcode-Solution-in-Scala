  object Solution {
    def longestNiceSubarray(nums: Array[Int]): Int = {
      import scala.annotation.tailrec
      @tailrec
      def traverse(start: Int, end: Int, cOr: Int, len: Int, max: Int): Int = {
        if (end == nums.length)
          max
        else {
          val next = nums(end)
          if ((cOr & next) == 0) {
            traverse(
              start,
              end + 1,
              cOr | next,
              len + 1,
              Math.max(max, len + 1)
            )
          } else {
            if (start + 1 == end) {
              traverse(start + 1, end + 1, nums(start + 1), 1, max)
            } else {
              traverse(start + 1, end, cOr ^ nums(start), len - 1, max)
            }
          }
        }
      }
      traverse(0, 1, nums(0), 1, 1)
    }
  }
