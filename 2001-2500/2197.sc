object Solution {
    def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a%b)

  def updateStack(n: Int, stack: List[Int]): List[Int] = stack match {
    case h :: t =>
      val GCD = gcd(n, h)
      if (GCD == 1) n :: stack else updateStack(((n.toLong*h)/GCD).toInt, t)
    case Nil => n :: stack
  }

  def replaceNonCoprimes(nums: Array[Int]): List[Int] = {
    def loop(index: Int, stack: List[Int]): List[Int] = {
      //print(index, stack); println()
      if (index == nums.length) stack.reverse
      else {
        val num = nums(index)
        loop(index+1, updateStack(num, stack))
      }
    }

    loop(0, Nil)
  }
}
