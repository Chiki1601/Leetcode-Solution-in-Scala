object Solution {
     	  def checkInclusion(s1: String, s2: String): Boolean = {
        val len1 = s1.length
        val len2 = s2.length
        if(len1 == 0) true
        else if(len2 == 0) len1 == 0
        else if(s1.length > s2.length) false
        else {
            val toCheckCount = s1.groupBy(ch => ch).map(kv => kv._1->kv._2.size)
            val currCount = scala.collection.mutable.Map[Char,Int]().withDefaultValue(0)
            
            s2.substring(0,len1).foreach{ch =>
                currCount(ch) += 1
            }
            
            var found = isSameCount(toCheckCount,currCount)
            var i = len1
            while(i < len2 && !found){
                currCount(s2(i)) += 1
                currCount(s2(i-len1)) -= 1
                if(isSameCount(toCheckCount,currCount)){
                    found = true
                }
                i += 1
            }
            found
        }
    }
    
    def isSameCount(map1:Map[Char,Int],map2:scala.collection.mutable.Map[Char,Int]): Boolean = {
        !map1.exists(kv => map2(kv._1) != kv._2)
    }
        
    
}
