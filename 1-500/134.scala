object Solution {
    def canCompleteCircuit(gas: Array[Int], cost: Array[Int]): Int = {
        val diffs = gas.zip(cost).map(x => x._1 - x._2)

        val start = diffs.indices.foldLeft((0, 0)) { case ((index, tank), i) =>
            if (tank + diffs(i) < 0) (i + 1, 0) else (index, tank + diffs(i))
        }._1

        if (diffs.sum < 0) -1 else start
    }
}
