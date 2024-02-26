object Solution {
    def isPossibleToSplit(nums: Array[Int]): Boolean = {
     nums.groupBy(identity).mapValues(_.length).values.forall( _ < 3)

    }
}
