object Solution {
  def minimumRecolors(blocks: String, k: Int): Int = {
    def impl(seq: Seq[Char], acc: Int = k): Int = {
      if(seq.size < k) acc
      else impl(seq.tail, acc min seq.take(k).count(_ != 'B'))
    }
    impl(blocks)
  }
}
