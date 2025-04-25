object Solution {
  def countInterestingSubarrays(nums: List[Int], modulo: Int, k: Int): Long =
    nums.map(n => ((n % modulo)==k).compare(false).sign)
      .scanLeft(0)(_ + _).map(_ % modulo)
      .foldLeft((0L,Map.empty[Int,Int].withDefaultValue(0))){case ((acc,aMap),x) =>
        (acc+aMap((x-k+modulo)%modulo), aMap + (x -> (aMap(x)+1)))
      }._1
}
