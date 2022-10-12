object Solution {
    def largestPerimeter(nums: Array[Int]): Int = {
        val tmp = nums.sorted
        for(i <- tmp.length - 3 to 0 by -1)
            if (tmp(i) + tmp(i + 1) > tmp(i + 2)) return tmp(i) + tmp(i + 1) + tmp(i + 2)
        return 0
    }
}
