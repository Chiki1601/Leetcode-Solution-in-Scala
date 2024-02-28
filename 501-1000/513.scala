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
        def traversal(level: List[TreeNode], leftValue: Int): Int = {
            level match {
                case Nil    => leftValue
                case _      =>
                    val nextlevel = level.flatMap(nd => List(Option(nd.left), Option(nd.right)).flatten)
                    val newLeftValue = level.head.value
                    traversal(nextlevel, newLeftValue)
            }
        }

        traversal(List(root), root.value)
            
    }
}
