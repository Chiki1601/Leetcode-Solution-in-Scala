object Solution {
    def inorderTraversal(root: TreeNode): List[Int] = {
   
        val result = collection.mutable.ArrayBuffer[Int]()

        def FUN_traverse(node: TreeNode): Unit = {
            if (node != null) {
                // 1st: LEFT
                FUN_traverse(node.left)
                // 2nd: current node value
                result += node.value
                // 3rd: RIGHT 
                FUN_traverse(node.right)
            }
        }

        FUN_traverse(root)
        result.toList
    }
}
