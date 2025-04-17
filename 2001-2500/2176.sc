object Solution {

  def countPairs(nums: Array[Int], k: Int): Int = {
    import scala.collection.mutable.ArrayBuffer
    
    val track = Array.fill(101)(new ArrayBuffer[Int]())

    var count = 0

    for (i <- nums.indices) {
      val occurrences = track(nums(i))
      for (idx <- occurrences if (i * idx) % k == 0) {
        count += 1
      }
      occurrences.addOne(i)
    }

    count
  }

}
