object Solution {
    def sortColors(nums: Array[Int]): Unit = {
        val ns =nums.sorted
        for (i <- 0 until nums.length){
            nums(i) =ns(i)
        }
    }
}
