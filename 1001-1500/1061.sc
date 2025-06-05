object Solution {
    def smallestEquivalentString(s1: String, s2: String, baseStr: String): String = {
        val unionFind = new UnionFind()
        s1.zip(s2).foreach {
            case (c1, c2) => unionFind.union(c1.asNum, c2.asNum)
        }
        baseStr.map(c => unionFind.find(c.asNum).asChar)
    }

    implicit class CharToNum(c: Char) {
        def  asNum: Int = c - 'a'
    }

    implicit class NumToChar(i: Int) {
        def asChar: Char = (i + 'a').toChar
    }

    class UnionFind {
        val parent = (0 until 26).toArray

        def find(i: Int): Int = {
            if (parent(i) == i) i
            else {
                parent(i) = find(parent(i))
                parent(i)
            }
        }

        def union(i: Int, j: Int): Unit = {
            val irep = find(i)
            val jrep = find(j)
            parent(math.max(irep, jrep)) = math.min(irep, jrep)
        }
    }
}
