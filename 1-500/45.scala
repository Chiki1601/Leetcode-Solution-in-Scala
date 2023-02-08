object Solution {
    def jump(nums: Array[Int]): Int = {
    nums.indices.foldLeft((0,0,0))((acc, i) =>{
      if(i == nums.length - 1) return i
      val next = acc._2 max i + nums(i)
      if(next >= nums.length - 1) return 1 + acc._1
      if(i == acc._3) (acc._1 + 1, next, next)
      else (acc._1, next, acc._3)
    })._1
  }
}
