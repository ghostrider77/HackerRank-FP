package memoization

object Expressions {
  import scala.annotation.tailrec

  implicit class RichInteger(k: Int) {
    def mod(m: Int): Int = {
      val remainder: Int = k % m
      if (remainder < 0) remainder + m else remainder
    }
  }

  private val Modulus: Int = 101

  private val AllowedOperations: Map[Char, (Int, Int) => Int] =
    Map(
      '*' -> ((a, b) => a * b),
      '+' -> ((a, b) => a + b),
      '-' -> ((a, b) => a - b)
    )

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def extendExpressions(expressions: Map[Int, List[Char]], elem: Int): Map[Int, List[Char]] =
    for {
      (remainder, operations) <- expressions
      (opChar, opFunc) <- AllowedOperations
    } yield opFunc(remainder, elem).mod(Modulus) -> (opChar :: operations)

  private def assembleExpression(numbers: List[Int], length: Int, operations: List[Char]): String = {
    val numberOfOperations: Int = operations.length
    val finalOperations: List[Char] =
      if (numberOfOperations == length - 1) operations.reverse
      else operations.reverse ::: List.fill[Char](length - 1 - numberOfOperations)('*')

    (numbers.head.toString :: numbers.tail.zip(finalOperations)
      .flatMap{ case (number, op) => List(op.toString, number.toString) })
      .mkString
  }

  def findExpression(numbers: List[Int], length: Int): String = {
    @tailrec
    def loop(ns: List[Int], expressions: Map[Int, List[Char]]): List[Char] = ns match {
      case Nil => expressions(0)
      case n :: nss =>
        val newExpressions: Map[Int, List[Char]] = extendExpressions(expressions, n)
        if (newExpressions.keySet.contains(0)) newExpressions(0)
        else loop(nss, newExpressions)
    }

    val operations: List[Char] = loop(numbers.tail, Map(numbers.head -> Nil))
    assembleExpression(numbers, length, operations)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val numbers: List[Int] = convertToIntList(reader.next())
    val result: String = findExpression(numbers, n)
    println(result)
  }
}
