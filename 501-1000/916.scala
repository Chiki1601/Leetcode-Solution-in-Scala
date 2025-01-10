object Solution {
 def wordSubsets(words1: Array[String], words2: Array[String]): List[String] = {
  val reqs = ('a' to 'z').map(l => l -> words2.map(_.count(_ == l)).max)
  words1.toList.filter(w => reqs.forall { case (l, c) => w.count(_ == l) >= c } )
}
}
