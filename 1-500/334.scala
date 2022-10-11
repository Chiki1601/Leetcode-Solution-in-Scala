  object Solution {
    def increasingTriplet(nums: Array[Int]): Boolean = {
      def increasingTriplet(index: Int, left: Int, mid: Int): Boolean = {
        if (index >= nums.length) false
        else if (mid < nums(index)) true
        else {
          val current: Int = nums(index)
          if (current < left) increasingTriplet(index + 1, current, mid)
          else if (current < mid && current > left) increasingTriplet(index + 1, left, current)
          else increasingTriplet(index + 1, left, mid)
        }
      }

      increasingTriplet(0, Int.MaxValue, Int.MaxValue)
    }
  }
