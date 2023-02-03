object Solution {
   def convert(s: String, numRows: Int): String = {
     if (numRows == 1) {
       s
     } else {
       val cycle = 2 * (numRows - 1)
       Range(0, numRows).flatMap { row =>
         Range(row, s.size).filter(i => (i%cycle) == row || (i%cycle) == cycle-row)
       }.map(s(_)).mkString
     }
   }
}
