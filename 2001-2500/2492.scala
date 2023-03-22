object Solution {
  val MAXDJS = 100000
  val MAXDIST = 10_001
  val djs = (0 to MAXDJS).map(_ -> MAXDIST).toArray
  def minScore(n: Int, roads: Array[Array[Int]]): Int = {
    (0 to n).foreach{i => djs(i) = (i -> MAXDIST)}
    def compress(i: Int, p: Int): Unit = if(i!=p) {val oldP = djs(i)._1; djs(i) = djs(p); compress(oldP,p)}
    def _root(i: Int): Int = if(djs(i)._1 == i) djs(i)._1 else _root(djs(i)._1)
    def root(i: Int): Int = {val r = _root(i); compress(i,r); r}
    def join(i: Int, j: Int, d: Int): Unit = {
      lazy val newRoot = (root(i), d min djs(root(i))._2 min djs(root(j))._2 )
      djs(root(j)) = newRoot; djs(root(i)) = newRoot
      djs(i) = newRoot; djs(j) = newRoot
    }
    roads.iterator.foreach{a => join(a(0),a(1),a(2))}
    djs(1)._2
  }
}
