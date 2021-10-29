package adhoc

object HugeGCD {
  import scala.annotation.tailrec
  import scala.collection.mutable.ListBuffer

  type CanonicalForm = Map[Int, Int]

  private val Modulus: Int = 1e9.toInt + 7
  private val UpperLimit: Int = 1e4.toInt
  private val Primes: List[Int] = getPrimes(UpperLimit)

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def readFactors(reader: Iterator[String]): List[Int] = {
    val _: Int = reader.next().toInt
    convertToIntList(reader.next())
  }

  private def getPrimes(limit: Int): List[Int] = {
    def isPrime(n: Int, primes: Iterator[Int]): Boolean = primes.takeWhile(_ <= math.sqrt(n)).forall(p => n % p != 0)
    val primes: ListBuffer[Int] = ListBuffer(2)
    for {
      p <- 3 to limit by 2
      if isPrime(p, primes.iterator)
    } primes += p

    primes.toList
  }

  private def findLargestExponent(n: Int, prime: Int): (Int, Int) = {
    @tailrec
    def loop(k: Int, exponent: Int): (Int, Int) = {
      if (k % prime != 0) (k, exponent)
      else loop(k / prime, exponent + 1)
    }
    loop(n, 0)
  }

  private def calcPrimeFactorization(n: Int): CanonicalForm = {
    @tailrec
    def loop(k: Int, primes: List[Int], factorization: CanonicalForm): CanonicalForm = primes match {
      case Nil if k > 1 => throw new Exception(s"$n is not factorized")
      case Nil => factorization
      case p :: ps =>
        if (k == 1) factorization
        else {
          val (remainder, exponent): (Int, Int) = findLargestExponent(k, p)
          if (exponent == 0) loop(remainder, ps, factorization)
          else loop(remainder, ps, factorization + (p -> exponent))
        }
    }
    loop(n, Primes, Map())
  }

  private def calcCanonicalForm(numbers: List[Int]): CanonicalForm = {
    def merge(acc: CanonicalForm, factors: CanonicalForm): CanonicalForm = {
      (for {
        p <- acc.keySet.union(factors.keySet).iterator
      } yield p -> (acc.getOrElse(p, 0) + factors.getOrElse(p, 0))).toMap
    }

    numbers.foldLeft(Map(): CanonicalForm)((acc, n) => if (n == 1) acc else merge(acc, calcPrimeFactorization(n)))
  }

  def calcGCGModulo(xs: List[Int], ys: List[Int]): Long = {
    val factorsOfN: CanonicalForm = calcCanonicalForm(xs)
    val factorsOfM: CanonicalForm = calcCanonicalForm(ys)
    val commonPrimes: Set[Int] = factorsOfN.keySet.intersect(factorsOfM.keySet)
    def findSmallestExponent(p: Int): Int = math.min(factorsOfN.getOrElse(p, 0), factorsOfM.getOrElse(p, 0))
    commonPrimes.foldLeft(1L){ (acc, p) =>
      val exponent: Int = findSmallestExponent(p)
      (0 until exponent).foldLeft(acc)((prod, _) => prod * p % Modulus)
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val factorsOfN: List[Int] = readFactors(reader)
    val factorsOfM: List[Int] = readFactors(reader)
    val result: Long = calcGCGModulo(factorsOfN, factorsOfM)
    println(result)
  }
}
