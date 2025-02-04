object Solution {
  def maxAscendingSum(nums: Array[Int]): Int =
    nums.iterator.drop(1)
      .scanLeft((nums.head,nums.head)){case ((acc,prev),next) =>
        if(next>prev) (acc+next,next) else (next,next) 
      }.map(_._1).max
}
