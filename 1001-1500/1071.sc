object Solution {
  import scala.annotation.tailrec

  @tailrec
  def gcdOfStrings(str1: String, str2: String): String =
    if (str1 + str2 != str2 + str1) ""
    else if (str1 == str2) str1
    else if (str1.length > str2.length)
      gcdOfStrings(str1.replaceFirst(str2, ""), str2)
    else
      gcdOfStrings(str1, str2.replaceFirst(str1, ""))
}
