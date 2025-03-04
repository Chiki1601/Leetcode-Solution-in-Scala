object Solution {
    def checkPowersOfThree(n: Int): Boolean = {
        var num = n
        while (num > 0) {
            if (num % 3 == 2) {
                return false
            }
            num /= 3
        }
        true
    }
}
