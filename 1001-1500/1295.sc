object Solution {
    def findNumbers(nums: Array[Int]): Int = {
        nums.count(value => value == 100000 || (value >= 1000 && value < 10000) ||
            (value >= 10 && value < 100))
    }
}
