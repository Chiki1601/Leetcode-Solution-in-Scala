object Solution {
    def rearrangeArray(nums: Array[Int]): Array[Int] = {
        @annotation.tailrec
        def go(i: Int, pos: Array[Int], neg: Array[Int], acc: List[Int]): Array[Int] = 
            if (i < 0) 
                acc.toArray
            else
                go(i-1, pos, neg, pos(i)::neg(i)::acc)

        val (pos, neg) = nums.partition(_ > 0)

        go(nums.length/2 - 1, pos, neg, List())
    }
}
