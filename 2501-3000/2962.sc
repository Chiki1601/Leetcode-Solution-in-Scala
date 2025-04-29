object Solution {
    def countSubarrays(nums: Array[Int], k: Int): Long = {
        recursiveCount(nums, 0, 0, k, 0L, 0, nums.max)
    }

    def recursiveCount(nums: Array[Int], i: Int, j: Int, k: Int,
        count: Long, freq: Int, max: Int): Long = {
        if (i == nums.length && freq < k)
            count
        else if (i < nums.length && freq < k) {
            val newFreq = if (nums(i) == max) freq + 1 else freq
            recursiveCount(nums, i + 1, j, k, count, newFreq, max)
        } else {
            val newCount = count + (nums.length - i + 1)
            val newFreq = if (nums(j) == max) freq - 1 else freq
            recursiveCount(nums, i, j + 1, k, newCount, newFreq, max)
        }
    }
}
