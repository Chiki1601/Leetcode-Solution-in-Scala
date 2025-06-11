object Solution {
    def maxDifference(s: String, k: Int): Int = {
        val length = s.length
        var result = Int.MinValue

        for (first <- 0 until 5) {
            for (second <- 0 until 5 if first != second) {
                val diff = Array.fill(length + 1)(0)
                val parityA = Array.fill(length + 1)(0)
                val parityB = Array.fill(length + 1)(0)
                val countB = Array.fill(length + 1)(0)

                for (i <- 1 to length) {
                    val digit = s.charAt(i - 1) - '0'
                    diff(i) = diff(i - 1) + (if (digit == first) 1 else 0) - (if (digit == second) 1 else 0)
                    parityA(i) = (parityA(i - 1) + (if (digit == first) 1 else 0)) & 1
                    parityB(i) = (parityB(i - 1) + (if (digit == second) 1 else 0)) & 1
                    countB(i) = countB(i - 1) + (if (digit == second) 1 else 0)
                }

                val storage = Array.fill(2, 2)(new MinBIT(length + 1))

                for (j <- 0 to length) {
                    if (j >= k) {
                        val back = j - k
                        val pA = parityA(back)
                        val pB = parityB(back)
                        val bCount = countB(back)
                        storage(pA)(pB).insert(bCount, diff(back))
                    }

                    if (j > 0 && countB(j) > 0) {
                        val altA = 1 - parityA(j)
                        val curB = parityB(j)
                        val minPrev = storage(altA)(curB).getMin(countB(j) - 1)

                        if (minPrev != MinBIT.MAX) {
                            result = math.max(result, diff(j) - minPrev)
                        }
                    }
                }
            }
        }

        if (result == Int.MinValue) 0 else result
    }

    class MinBIT(val size: Int) {
        private val data = Array.fill(size + 2)(MinBIT.MAX)

        def insert(index: Int, value: Int): Unit = {
            var i = index + 1
            while (i <= size) {
                data(i) = math.min(data(i), value)
                i += (i & -i)
            }
        }

        def getMin(index: Int): Int = {
            var res = MinBIT.MAX
            var i = index + 1
            while (i > 0) {
                res = math.min(res, data(i))
                i -= (i & -i)
            }
            res
        }
    }

    object MinBIT {
        val MAX: Int = Int.MaxValue
    }
}
