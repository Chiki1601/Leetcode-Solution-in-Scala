object Solution {
  def countSubarrays(nums: Array[Int], k: Long): Long = {
    def f(l:Int,r:Int,sum:Long,acc:Long):Long = {
      lazy val prod = sum*(r-l)
      if(r>=nums.length && prod<k) acc+(r-l)
      else if(r>=nums.length) f(l+1,r,sum-nums(l),acc)
      else if(l>=r) f(l,l+1,nums(l),acc)
      else if(prod<k) f(l,r+1,sum+nums(r),acc+(r-l))
      else f(l+1,r,sum-nums(l),acc)
    }
    f(0,0,0L,0L)
  }
}
