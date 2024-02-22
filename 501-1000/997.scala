object Solution {
    def findJudge(n: Int, trust: Array[Array[Int]]): Int = {

        val trustAmount = Array.fill(n + 1)(0)
        val gainedTrustAmount = Array.fill(n + 1)(0)
        
        for(Array(personWhoTrust, personWhoGainedTrust) <- trust) {
            trustAmount(personWhoTrust) += 1
            gainedTrustAmount(personWhoGainedTrust) += 1
        }

        for {
            idx <- 1 until trustAmount.length
            // judge trusts nobody && has n - 1 (not counting themselves) amount of trust
            if(trustAmount(idx).equals(0) && gainedTrustAmount(idx).equals(n - 1))
        } return idx

        -1
    }
}
