  object Solution {
    def numberOfSubstrings(word: String): Int = {
      extension (counts: Array[Int]) {
        def addLetter(l: Char): Unit = {
          counts(l - 'a') += 1
        }
        def remLetter(l: Char): Unit = {
          counts(l - 'a') -= 1
        }
        def cons: Int = counts(0)
        def isValid: Boolean =
          counts(0) >= 1 &&
            counts(1) >= 1 &&
            counts(2) >= 1
      }

      class Window(_l: Int = 0) {
        var leftEdge = _l
        val counts: Array[Int] = new Array(3)
        def shrink(): Unit = {
          counts.remLetter(word(leftEdge))
          leftEdge += 1
        }
        def myclone(): Window = {
          val w = new Window(leftEdge)
          w.copyOf(this)
          w
        }
        def copyOf(w: Window): Unit = {
          leftEdge = w.leftEdge
          counts(0) = w.counts(0)
          counts(1) = w.counts(1)
          counts(2) = w.counts(2)
        }
        def isValid: Boolean = counts.isValid
        def cons: Int = counts.cons
      }

      var r = 0
      var tot: Long = 0L
      var ll = new Window() // for a given r, ll is the largest valid window
      var lr =
        new Window() // for a given r, lr is the smallest valid window, but with index + 1 to make calculation easier

      while (r < word.size) {
        // grow the windows
        ll.counts.addLetter(word(r))
        lr.counts.addLetter(word(r))
        r += 1

        // Then count the # of valid substrings
        if (ll.isValid) { // ll is valid
          // don't want to do lr.copyOf(ll) here so that we can reuse the previous lr for when there are many vowels in a row
          while (lr.isValid) {
            lr.shrink()
          }
          tot += lr.leftEdge - ll.leftEdge
        }
      }

      tot.toInt
    }
  }
