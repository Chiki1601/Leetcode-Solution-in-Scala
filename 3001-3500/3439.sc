object Solution {
  import scala.collection.immutable.Queue
  def maxFreeTime(eventTime: Int, k: Int, startTime: Array[Int], endTime: Array[Int]): Int = {
    val (t,d) = (List(0) ++ (
      startTime zip endTime).flatMap{case (a,b) => Iterator(a,b)} ++
      Iterator.continually(eventTime).take(k) )
      .sliding(2,2).map(a => a.last - a.head).to(Queue)
      .splitAt(k+1)
    d.iterator.scanLeft((t.sum,t)){case ((acc,q),x) =>
      (acc-q.head+x,q.tail :+ x)
    }.map(_._1).max
  }
}
