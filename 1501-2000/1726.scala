object Solution {
  def tupleSameProduct(nums: Array[Int]): Int =
    nums.toList.tails.filter(_.length>1)
    .flatMap{l => l.tail.map(l.head * _)}
    .toList.groupMapReduce(identity)(_ => 1)(_ + _).values
    .collect{case n if n>1 => 4*n*(n-1)}.sum
}
