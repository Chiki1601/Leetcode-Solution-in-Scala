import scala.collection.mutable.{PriorityQueue, ListBuffer}
import scala.math.min

final case class Flight(city: Int, cost: Int, stop: Int)

object Solution {
    def findCheapestPrice(n: Int, flights: Array[Array[Int]], src: Int, dst: Int, k: Int): Int = {

        val adjacent = Array.fill(n)(new ListBuffer[Flight])
        val djikstra = Array.fill(n)(Int.MaxValue)

        for(Array(from, to, price) <- flights) adjacent(from) += Flight(to, price, 0)

        val start = Flight(src, 0, 0)
        val queue = PriorityQueue[Flight](start)(Ordering.by(_.cost))

        while(queue.nonEmpty) {

            val Flight(currentCity, currentCost, currentStops) = queue.dequeue

            if(currentCity == dst && currentCost < djikstra(currentCity) && currentStops <= k + 1) djikstra(currentCity) = currentCost
            else if(currentStops <= k) {

                for(Flight(destCity, destCost, _) <- adjacent(currentCity) if(currentCost + destCost < djikstra(destCity))) {
                    
                    val newCost = currentCost + destCost
                    djikstra(destCity) = newCost
                    val newFlight = Flight(destCity, newCost, currentStops + 1)
                    queue.enqueue(newFlight)

                }

            }
        }

        if(djikstra(dst) != Int.MaxValue) djikstra(dst) else -1
    }
}
