object Solution {
  lazy val map1 = "aeiou".zipWithIndex.to(Map).withDefaultValue(-1)
  def countOfSubstrings(word: String, k: Int): Long = {
    case class State(var cs: Int = 0, vs: Array[Int] = Array.fill("aeiou".length)(0), var idx: Int = 0) extends Iterator[State] {
      def hasAllVs(that: State): Boolean = (this.vs.iterator zip that.vs.iterator).iterator.forall { case (a, b) => (a - b) != 0 }
      override def hasNext(): Boolean = idx <= word.length
      override def next(): State = {
        if(idx >= word.length) {idx = Int.MaxValue}
        else {if (map1.keySet.contains(word(idx))) vs(map1(word(idx))) += 1 else cs += 1; idx += 1}
        this
      }
    }
    def f(i0: State, i1: State, j: State, acc: Long): Long = {
      if (!j.hasNext()) acc
      else if (j.cs - i0.cs < k) f(i0, i1, j.next(), acc)
      else if (j.cs - i0.cs > k) f(i0.next(), i1, j, acc)
      else if (i1.cs < i0.cs) f(i0, i1.next(), j, acc)
      else if (!j.hasAllVs(i0)) f(i0, i1, j.next(), acc)
      else if (i0.cs==i1.cs && j.hasAllVs(i1)) f(i0, i1.next(), j, acc)
      else f(i0, i1, j.next(), acc+(i1.idx-i0.idx))
    }
    f(State(),State(),State(),0L)
  }
}
