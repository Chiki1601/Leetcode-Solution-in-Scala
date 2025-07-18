import scala.collection.mutable

object Solution {
  def minimumDifference(nums: Array[Int]): Long = {
    val n = nums.length / 3
    val leftSums = {
      val queue = mutable.PriorityQueue().addAll(nums.iterator.take(n))
      (n until 2 * n).scanLeft(nums.iterator.map(_.toLong).take(n).sum) { (sum, i) =>
        queue += nums(i)
        sum - queue.dequeue + nums(i)
      }
    }
    val rightSums = {
      val queue = mutable.PriorityQueue()(Ordering.Int.reverse).addAll(nums.reverseIterator.take(n))
      (n until 2 * n).scanRight(nums.reverseIterator.map(_.toLong).take(n).sum) { (i, sum) =>
        queue += nums(i)
        sum - queue.dequeue + nums(i)
      }
    }
    leftSums.iterator.zip(rightSums).map(_ - _).min
  }
}
