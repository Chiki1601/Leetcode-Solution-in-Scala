object Solution {
    def findDifferentBinaryString(nums: Array[String]): String = {
      nums.indices.map(i => if (nums(i)(i) == '0') '1' else '0').mkString
    }
}
