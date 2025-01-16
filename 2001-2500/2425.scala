object Solution {
  def xorAllNums(nums1: Array[Int], nums2: Array[Int]): Int = {
    // XOR result initialized to 0
    val n1 = nums1.length
    val n2 = nums2.length

    // If one array has an even length, its contribution to the XOR cancels out
    val xor1 = if (n2 % 2 == 0) 0 else nums1.reduce(_ ^ _)
    val xor2 = if (n1 % 2 == 0) 0 else nums2.reduce(_ ^ _)

    // Combine results
    xor1 ^ xor2
  }
}
