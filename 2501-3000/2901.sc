object Solution {
  import scala.util.chaining._
  def getWordsInLongestSubsequence(words: Array[String], groups: Array[Int]): List[String] = {
    lazy val aMap = (for{
      i <- words.indices.to(List)
      j <- Iterator.range(i+1,words.length)
      if words(i).size == words(j).size && groups(i) != groups(j) && 
        (words(i) zip words(j)).map{case (a,b) => (a-b).sign.abs}.sum == 1
    } yield {(i,j)}).groupMap(_._1)(_._2)
    lazy val cMap = words.indices.reverse.foldLeft(Map.empty[Int,Int]){case (bMap,i) =>
      bMap + (i -> (aMap.getOrElse(i,Nil).flatMap(bMap.get).maxOption.getOrElse(0)+1))
    }
    List.unfold(cMap.iterator.maxBy(_._2)._1){case i => Option.when(i>=0){
        (words(i), aMap.get(i).toList.flatten.maxByOption(cMap).getOrElse(-1))
    }}
  }
}
