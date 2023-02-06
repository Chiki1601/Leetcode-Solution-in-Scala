object Solution {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {

    def check(current: Int, cursor: Int): Array[Int] = {

      if (current > nums.length) Array.emptyIntArray
      else if (current != cursor && nums(current) + nums(cursor) == target) Array(current,cursor)      
      else if (cursor < nums.length-1) check(current, cursor+1)      
      else check(current+1, 1)
      
    }

    check(0,1)

  }
}
