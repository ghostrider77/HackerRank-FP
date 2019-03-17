package adhoc

object RotateString {
  def getRotations(string: String): Iterator[String] = {
    val length: Int = string.length
    val doubleString = string + string
    doubleString.drop(1).sliding(length)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val strings: List[String] = reader.take(n).toList
    strings.map(getRotations).foreach(rotations => println(rotations.mkString(" ")))
  }
}
