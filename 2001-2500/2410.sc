object Solution {
  def matchPlayersAndTrainers(players: Array[Int], trainers: Array[Int]): Int = {
    val sortedPlayers = players.sorted
    val sortedTrainers = trainers.sorted

    var i, j, matches = 0
    while (i < sortedPlayers.length && j < sortedTrainers.length) {
      if (sortedPlayers(i) <= sortedTrainers(j)) {
        matches += 1
        i += 1
      }
      j += 1
    }
    matches
  }
}
