object Solution {
    def checkIfPangram(sentence: String): Boolean = {
        sentence.length >= 26 && sentence.toSet == {'a' to 'z'}.toSet
    }
}
