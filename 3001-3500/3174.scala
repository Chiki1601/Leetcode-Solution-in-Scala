object Solution {
  def clearDigits(s: String): String =
    s.to(List).reverse.scanLeft((Option.empty[Char],0)){case ((_,cnt),c) =>
      if(c.isDigit) (None, cnt+1)
      else if(cnt>0) (None, cnt-1)
      else (Some(c), 0)
    }.flatMap(_._1).reverse.mkString
}
