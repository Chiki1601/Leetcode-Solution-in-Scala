  object Solution {
    var doLog = false
    def println(a: Any): Unit =
      if (doLog)
        Console.out.println(a)
    def lcaDeepestLeaves(root: TreeNode): TreeNode = {
      def helper(tn: TreeNode, depth: Int): (Int, TreeNode) = {
        val (ldepth, llca) =
          Option(tn.left).map(helper(_, depth + 1)).getOrElse(-1, tn)
        val (rdepth, rlca) =
          Option(tn.right).map(helper(_, depth + 1)).getOrElse(-1, tn)
        if (ldepth == rdepth && ldepth == -1) (depth, tn)
        else if (ldepth == rdepth) (ldepth, tn)
        else if (ldepth < rdepth) (rdepth, rlca)
        else (ldepth, llca)
      }
      helper(root, 0)._2
    }
  }
