package memoization

object PasswordCracker {
  import scala.collection.mutable.{Map => MutableMap}

  final case class LoginAttempt(passwords: List[String], attempt: String)

  private def getStringRepresentation(result: Option[List[String]]): String = result match {
    case None => "WRONG PASSWORD"
    case Some(passwords) => passwords.mkString(" ")
  }

  def getPasswords(testCase: LoginAttempt): String = {
    val LoginAttempt(passwords, attempt) = testCase
    val dict: MutableMap[String, Option[List[String]]] = MutableMap("" -> Some(Nil))

    def decompose(string: String, acc: List[String]): Option[List[String]] = {
      if (string.isEmpty) Some(acc.reverse)
      else {
        def decomposition: Option[List[String]] = {
          val decompositionsOfString: List[List[String]] =
            for {
              password <- passwords
              if string.startsWith(password)
              suffix = string.stripPrefix(password)
              result <- decompose(suffix, password :: acc)
            } yield result
          decompositionsOfString match {
            case Nil => None
            case d :: _ => Some(d)
          }
        }
        dict.getOrElseUpdate(string, decomposition)
      }
    }

    getStringRepresentation(decompose(attempt, Nil))
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val numberOfTestCases: Int = reader.next().toInt
    val testCases: List[LoginAttempt] = (for { _ <- 0 until numberOfTestCases } yield {
      val _: Int = reader.next().toInt
      val passwords: List[String] = reader.next().split(" ").toList
      val attempt: String = reader.next()
      LoginAttempt(passwords, attempt)
    }).toList

    val results: List[String] = testCases.map(getPasswords)
    results.foreach(println)
  }
}
