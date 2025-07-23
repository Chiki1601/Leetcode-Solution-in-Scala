object Solution {
    def removePair(s: String, pair: Array[Char], value: Int): (String, Int) = {
        val sb = new StringBuilder
        var res = 0
        for (ch <- s) {
            if (ch == pair(1) && sb.nonEmpty && sb.charAt(sb.length - 1) == pair(0)) {
                sb.deleteCharAt(sb.length - 1)
                res += value
            } else {
                sb.append(ch)
            }
        }
        (sb.toString, res)
    }
    def maximumGain(s: String, x: Int, y: Int): Int = {
        def helper(first: Array[Char], second: Array[Char], firstValue: Int, secondValue: Int): Int = {
            val (intermediateString, res) = removePair(s, first, firstValue)
            val (finalString, finalRes) = removePair(intermediateString, second, secondValue)
            res + finalRes
        }
        if (x > y) {
            helper(Array[Char]('a', 'b'), Array[Char]('b', 'a'), x, y)
        } else {
            helper(Array[Char]('b', 'a'), Array[Char]('a', 'b'), y, x)
        }
    }
}
