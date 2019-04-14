package adhoc

object CaptainPrime {
  private def isPrime(n: Int): Boolean = {
    if (n == 1) false
    else if (n == 2) true
    else {
      val limit = math.sqrt(n).toInt + 1
      (2 to limit).forall(k => n % k != 0)
    }
  }

  private def isAllSubnumberPrime(id: Int, side: String): Boolean = {
    val stringId: String = id.toString
    val numberOfDigits: Int = stringId.length
    val primeCandidates: List[Int] = {
      if (side == "left") (0 until numberOfDigits).map(ix => stringId.drop(ix).toInt).tail.toList
      else (0 until numberOfDigits).map(ix => stringId.take(ix + 1).toInt).init.toList
    }
    primeCandidates.forall(isPrime)
  }

  private def getShipSide(id: Int): String = {
    val left: Boolean = isAllSubnumberPrime(id, side = "left")
    val right: Boolean = isAllSubnumberPrime(id, side = "right")
    if (left && right) "CENTRAL"
    else if (left) "LEFT"
    else if (right) "RIGHT"
    else "DEAD"
  }

  def calculateIdType(ids: List[Int]): List[String] =
    for { id <- ids } yield if (id.toString.contains("0") || !isPrime(id)) "DEAD" else getShipSide(id)

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrCases: Int = reader.next().toInt
    val ids: List[Int] = (for { _ <- 0 until nrCases } yield reader.next().toInt).toList
    val result: List[String] = calculateIdType(ids)
    result.foreach(println)
  }
}
