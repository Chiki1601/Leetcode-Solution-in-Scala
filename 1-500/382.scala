/**
 * Definition for singly-linked list.
 * class ListNode(_x: Int = 0, _next: ListNode = null) {
 *   var next: ListNode = _next
 *   var x: Int = _x
 * }
 */
  class Solution(_head: ListNode) {

    private val list = new scala.collection.mutable.ListBuffer[Int]()
    private var current: ListNode = _head

    while (current != null) {
      list += current.x
      current = current.next
    }

    private val rand = new scala.util.Random()

    def getRandom(): Int = {

      val r = rand.nextInt(list.size)

      list(r)
    }
  }

/**
 * Your Solution object will be instantiated and called as such:
 * var obj = new Solution(head)
 * var param_1 = obj.getRandom()
 */
