/**
 * Definition for a binary tree node.
 * class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
 *   var value: Int = _value
 *   var left: TreeNode = _left
 *   var right: TreeNode = _right
 * }
 */
object Solution {
    def findBottomLeftValue(root: TreeNode): Int = {
            def findDepth(node: TreeNode):Int = {
       node match{
         case null => 0
         case _ => math.max(findDepth(node.left), findDepth(node.right)) + 1
       }
     }

  def helper(node: TreeNode, remainDepth: Int) :Option[Int] = {
    (node, remainDepth) match {
      case (null, _) => None
      case (_, 1) => Some(node.value)
      case (n,d) => {
        var left = helper(n.left, d-1)
        if (!left.isDefined) helper(n.right, d-1)
        else Some(left.get)
      } 
    }
  }
  val depth = findDepth(root)
  helper(root, depth).get
    }
}
