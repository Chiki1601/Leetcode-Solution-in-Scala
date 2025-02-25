  object Solution {
    def numOfSubarrays(arr: Array[Int]): Int = {
      var total = 0
      val MOD = 1000000007
      var evens = 0
      var odds = 0
      var isOddC = false
      var ind = 0
      while (ind < arr.length) {
        val i = arr(ind)
        val isOdd = (i & 1) != 0

        if (isOddC) {
          odds += 1
        } else {
          evens += 1
        }
        if (isOdd ^ isOddC) {
          total = (total + evens) % MOD
        } else {
          total = (total + odds) % MOD
        }
        isOddC ^= isOdd
        ind += 1
      }
      total
    }
  }
