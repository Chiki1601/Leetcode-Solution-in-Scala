object Solution {
    def numberOfPairs(points: Array[Array[Int]]): Int = {
        // Sort by x ascending, and if x is same then by y descending
        val sorted = points.sortWith { (a, b) =>
            if (a(0) == b(0)) a(1) > b(1) else a(0) < b(0)
        }

        val ys = sorted.map(_(1))
        var result = 0

        for (i <- ys.indices) {
            var maxY = Int.MinValue
            for (j <- (i + 1) until ys.length if ys(i) >= ys(j)) {
                if (maxY < ys(j)) {
                    result += 1
                    maxY = ys(j)
                }
            }
        }

        result
    }
}
