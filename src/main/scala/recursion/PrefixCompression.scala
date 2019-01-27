package recursion

object PrefixCompression {
  final case class Result(commonPrefix: String, suffix1: String, suffix2: String)

  def findCommonPrefix(s1: List[Char], s2: List[Char]): Result = {
    val prefixLength: Int = s1.zip(s2).prefixLength{ case (x, y) => x == y }
    Result(s1.take(prefixLength).mkString, s1.drop(prefixLength).mkString, s2.drop(prefixLength).mkString)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val s1: String = reader.next()
    val s2: String = reader.next()
    val Result(prefix, suffix1, suffix2) = findCommonPrefix(s1.toList, s2.toList)
    println(prefix.length + " " + prefix)
    println(suffix1.length + " " + suffix1)
    println(suffix2.length + " " + suffix2)
  }
}
