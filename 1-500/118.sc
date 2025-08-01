object Solution {

  import scala.collection.mutable.ListBuffer

  def generate(numRows: Int): List[List[Int]] = {
    def gen(prev: List[Int]): List[Int] = {
      prev.size match {
        case 1 => List(1, 1)
        case _ => 1 :: prev.sliding(2).map(_.sum).toList ::: List(1)
      }
    }
    
    val rows = ListBuffer(List(1))

    var rowIdx = 1
    var prevRow = List(1)
    while (rowIdx < numRows) {
      prevRow = gen(prevRow)
      rows.append(prevRow)
      rowIdx += 1
    }
    rows.result()
  }
}
