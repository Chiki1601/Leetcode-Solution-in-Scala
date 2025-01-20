import scala.util.control.Breaks._

object Solution {
    def firstCompleteIndex(arr: Array[Int], mat: Array[Array[Int]]): Int = {
        
        val map = scala.collection.mutable.Map[Int, (Int, Int)]()
        
        // Mapping the values to their row and column positions
        for (i <- mat.indices; j <- mat(i).indices) {
            map(mat(i)(j)) = (i, j)
        }

        val rows = mat.length
        val columns = mat(0).length
        
        val testingRows = Array.fill(rows)(0)
        val testingColumns = Array.fill(columns)(0)
        
        var result = -1
        breakable {
            // Traverse through the elements in the array arr
            for (i <- arr.indices) {
                val (r, c) = map(arr(i))
                
                if ({testingRows(r) += 1; testingRows(r)} == columns) {
                    result = i
                    break()  // Breaks out of the loop and ends execution
                }

                if ({testingColumns(c) += 1; testingColumns(c)} == rows) {
                    result = i
                    break()  // Breaks out of the loop and ends execution
                }
            }
        }

        result
    }
}
