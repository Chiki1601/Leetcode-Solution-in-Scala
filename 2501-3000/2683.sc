object Solution {
    def doesValidArrayExist(derived: Array[Int]): Boolean = {
        derived.reduce(_ ^ _) == 0
    }
}
