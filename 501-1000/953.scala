object Solution {
   	def isAlienSorted(words: Array[String], order: String): Boolean = {
        val indexes = Array.fill(26)(-1)
        //Get the order index for each char
        order.zipWithIndex.foreach{case(ch,i) =>
            if(indexes(ch-'a') == -1) indexes(ch-'a') = i
        }
        var isValid = true
        var j = 0
        while(j < words.length && isValid){
                if(j > 0){ // check curr and prev word now
                    var shouldContinue = true
                    var i = 0
                    while(i < scala.math.min(words(j).length,words(j-1).length) && shouldContinue && isValid){ // i=0 to min length of curr and prev string
                        val first = words(j-1)(i)
                        val second = words(j)(i)
                        if(indexes(first-'a') > indexes(second-'a')) {//Out of order
                            isValid = false    
                        } else if(indexes(first-'a') < indexes(second-'a')){
                            shouldContinue = false//If diff chars and in correct order, no need to check further
                        }
                        i += 1
                    }
                    //If shouldContinue=true, means equal chars in both string
                    if(shouldContinue && words(j).length < words(j-1).length) isValid = false // case apple vs app
                }
                j += 1
            }
        isValid
    }
}
