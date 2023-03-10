class Solution(_head: ListNode) {

    /** @param head The linked list's head.
        Note that the head is guaranteed to be not null, so it contains at least one node. */

    /** Returns a random node's value. */
    def getRandom(): Int = {
 
        var node=_head
        var length=1
        while (node.next != null)
        {
            length=length+1
            node=node.next
        }
        node=_head
/*        var r=new scala.util.Random()
        for (i<-0 until r.nextInt(length))
            {
                if (node.next != null)
                    node=node.next
            }
*/
        
        for (i<- 0 until scala.math.floor(math.random().toFloat*length).toInt)
            {
                if (node.next != null)
                    node=node.next
            }
        
        return node.x
        
    }

}
