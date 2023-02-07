var max = 0;
    def totalFruit(fruits: Array[Int]): Int = {
        max = 0
        check(0, fruits, List[Int](), 0, 0)
        max
    }
   def check(start: Int, fruits: Array[Int], checkFruits: List[Int], count: Int, startIndex: Int) : Unit = {

    if (start == fruits.length){
      if (max < count)
        max = count
    }
    else{
      if (checkFruits.length < 2){
         if (checkFruits.contains(fruits(start))){
          check(start + 1, fruits, checkFruits, count + 1, startIndex)
        }else{
          val newList = checkFruits :+ fruits(start)
          check(start + 1, fruits, newList, count + 1, start)
        }
      }else{
        if (checkFruits.contains(fruits(start))){
          check(start + 1, fruits, checkFruits, count + 1, startIndex)
        }else{
          check(startIndex, fruits, List[Int](), 0, startIndex)
        }
      }
      if (max < count)
          max = count
    }
  }
