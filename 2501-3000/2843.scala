object Solution {
  def countSymmetricIntegers(low: Int, high: Int): Int =
    (low to high).map{i =>
      lazy val s = i.toString.map(_.asDigit)
      lazy val (t,d) = s.splitAt(s.length/2)
      if(t.length == d.length && t.sum == d.sum) 1 else 0
    }.sum
}
