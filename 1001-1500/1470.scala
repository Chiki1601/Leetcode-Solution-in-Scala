object Solution {
    def shuffle(nums: Array[Int], n: Int): Array[Int] = {
        
        var arr = scala.collection.mutable.ArrayBuffer[Int]()
        
        for { 
            i <- 0 until n 
        } {
            arr += nums(i)
            if ((i + n) < nums.size) arr += nums(i + n)
        }
        
        arr.toArray
    }
}
