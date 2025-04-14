object Solution {
    def countGoodTriplets(arr: Array[Int], a: Int, b: Int, c: Int): Int = {
        (0 until arr.length - 2).flatMap(i =>
            (i + 1 until arr.length - 1).flatMap(j =>
                (j + 1 until arr.length).map(k =>
                    val v1 = math.abs(arr(i) - arr(j))
                    val v2 = math.abs(arr(j) - arr(k))
                    val v3 = math.abs(arr(i) - arr(k))
                    if (v1 <= a && v2 <= b && v3 <= c) 1 else 0
                )
            )
        ).sum
    }
}
