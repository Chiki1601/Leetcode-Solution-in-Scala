object Solution {
  def lengthAfterTransformations(s: String, t: Int, nums: List[Int]): Int = {
    val MOD = 1000000007
    val ALPHA = 26

    // Initialize transformation matrix
    val base: Array[Array[Int]] = Array.fill(ALPHA, ALPHA)(0)
    for ((count, i) <- nums.zipWithIndex) {
      for (k <- 1 to count) {
        val to = (i + k) % ALPHA
        base(i)(to) += 1
      }
    }

    // Convert string to counts vector
    val counts = Array.fill(ALPHA)(0L)
    for (ch <- s) {
      val idx = ch - 'a'
      counts(idx) += 1
    }

    // Matrix multiplication
    def mulMatrix(A: Array[Array[Int]], B: Array[Array[Int]]): Array[Array[Int]] = {
      val res = Array.fill(ALPHA, ALPHA)(0)
      for (i <- 0 until ALPHA; j <- 0 until ALPHA) {
        var sum = 0L
        for (k <- 0 until ALPHA) {
          sum = (sum + A(i)(k).toLong * B(k)(j)) % MOD
        }
        res(i)(j) = sum.toInt
      }
      res
    }

    // Multiply vector with matrix
    def mulVector(vec: Array[Long], mat: Array[Array[Int]]): Array[Long] = {
      val res = Array.fill(ALPHA)(0L)
      for (j <- 0 until ALPHA) {
        var sum = 0L
        for (i <- 0 until ALPHA) {
          sum = (sum + vec(i) * mat(i)(j)) % MOD
        }
        res(j) = sum
      }
      res
    }

    // Fast matrix exponentiation
    def matrixPow(mat: Array[Array[Int]], exp: Int): Array[Array[Int]] = {
      var res = Array.tabulate(ALPHA, ALPHA)((i, j) => if (i == j) 1 else 0)
      var m = mat.map(_.clone)
      var e = exp

      while (e > 0) {
        if ((e & 1) == 1) {
          res = mulMatrix(res, m)
        }
        m = mulMatrix(m, m)
        e >>= 1
      }
      res
    }

    val powerMat = matrixPow(base, t)
    val finalCounts = mulVector(counts, powerMat)
    (finalCounts.sum % MOD).toInt
  }
}
