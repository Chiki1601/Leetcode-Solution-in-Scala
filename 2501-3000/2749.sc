object Solution {
  def makeTheIntegerZero(num1: Int, num2: Int): Int = {
    LazyList.iterate(num1.toLong)(_ - num2).zipWithIndex.tail
    .takeWhile(_._1 >=0).take(35)
    .collect{case (n1,i) if BigInt(n1).bitCount <= i && i<=n1 => Some(i)}
    .flatten.headOption.getOrElse(-1)
  }
}
