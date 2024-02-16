object Solution {
      def findLeastNumOfUniqueInts(arr: Array[Int], k: Int): Int = {
    val groupedValues = arr.groupBy(identity)
    val queue = scala.collection.mutable.PriorityQueue.empty[(Int, Array[Int])](Ordering.by(_._2.length)).reverse ++ groupedValues

    (1 to k).map(_ => {
      val (key, value) = queue.dequeue()
      if(value.length > 1) queue += (key -> value.tail)
    })
    queue.length
  }
}
