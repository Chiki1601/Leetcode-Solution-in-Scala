  object Solution {
    def closestPrimes(left: Int, right: Int): Array[Int] = {
      // https://stackoverflow.com/questions/36882103/checking-whether-a-number-is-prime-in-scala/57645847#57645847
      def isPrime(number: Int): Boolean = {
        if (number < 4) number > 1
        else if (number % 2 == 0 || number % 3 == 0) false
        else
          (5 to math.sqrt(number).toInt by 6).forall(i =>
            number % i != 0 && number % (i + 2) != 0
          )
      }
      if (left <= 2) {
        if (right >= 3)
          Array(2, 3)
        else Array(-1, -1)
      } else
        ((left + (~left & 1)) to right by 2)
          .filter(isPrime)
          .sliding(2)
          .filter(_.size == 2) match {
          case x if x.isEmpty => Array(-1, -1)
          case x =>
            x
              .minBy(w => w(1) - w(0))
              .toArray
        }
    }
  }
