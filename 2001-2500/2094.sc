object Solution {
    def findEvenNumbers(digits: Array[Int]): Array[Int] = {
      digits.
        combinations(3)
        .flatMap(_.permutations)
        .map(_.mkString.toInt)
        .filter(num => num % 2 == 0 && num.toString.length == 3 && num.toString.head != '0')
        .toArray
        .distinct
        .sorted
    }
}
