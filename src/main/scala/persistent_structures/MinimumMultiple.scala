package persistent_structures

object MinimumMultiple {
  import scala.annotation.tailrec
  import scala.collection.immutable.HashMap
  import scala.collection.mutable.ListBuffer

  private val Modulus: Int = 1e9.toInt + 7
  private val UpperLimit: Int = 100
  private val Primes: List[Int] = getPrimes(UpperLimit)

  sealed trait Operation
  final case class Query(leftIx: Int, rightIx: Int) extends Operation
  final case class Update(ix: Int, multiplier: Int) extends Operation

  final case class CanonicalForm(primeFactorization: HashMap[Int, Int]) {
    lazy val asInt: Int = primeFactorization.foldLeft(1L) {
      case (acc, (p, exponent)) => (0 until exponent).foldLeft(acc)((prod, _) => prod * p % Modulus)
    }.toInt

    def *(that: CanonicalForm): CanonicalForm =
      CanonicalForm(this.primeFactorization.merged(that.primeFactorization){ case ((p, e1), (_, e2)) => (p, e1 + e2) })
  }

  object CanonicalForm {
    def apply(n: Int): CanonicalForm = {
      @tailrec
      def loop(k: Int, primes: List[Int], factorization: HashMap[Int, Int]): CanonicalForm = primes match {
        case Nil if k > 1 => throw new Exception(s"$n is not factorized!")
        case Nil => CanonicalForm(factorization)
        case p :: ps =>
          if (k == 1) CanonicalForm(factorization)
          else {
            val (remainder, exponent): (Int, Int) = findLargestExponent(k, p)
            if (exponent == 0) loop(remainder, ps, factorization)
            else loop(remainder, ps, factorization + (p -> exponent))
          }
      }
      loop(n, Primes, HashMap())
    }

    def leastCommonMultiple(form1: CanonicalForm, form2: CanonicalForm): CanonicalForm =
      CanonicalForm(form1.primeFactorization.merged(form2.primeFactorization){
        case ((p, e1), (_, e2)) => (p, math.max(e1, e2))
      })

    private def findLargestExponent(n: Int, prime: Int): (Int, Int) = {
      @tailrec
      def loop(k: Int, exponent: Int): (Int, Int) = {
        if (k % prime != 0) (k, exponent)
        else loop(k / prime, exponent + 1)
      }
      loop(n, 0)
    }
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

  private def convertToIntVector(line: String): Vector[Int] = line.split(" ").map(_.toInt).toVector

  def readOperation(line: String): Operation = line.split(" ").toList match {
    case List("Q", i, j) => Query(i.toInt, j.toInt)
    case List("U", ix, m) => Update(ix.toInt, m.toInt)
    case _ => throw new Exception("Unknown operation.")
  }

  private def calcSegmentTree(array: Vector[Int],
                              canonicalForms: Vector[CanonicalForm],
                              n: Int): Array[CanonicalForm] = {
    def log2(x: Double): Double = math.log(x) / math.log(2)
    val maxNrNodes: Int = 2 * math.pow(2, math.ceil(log2(n)).toInt).toInt - 1
    val segmentTree: Array[CanonicalForm] = Array.fill(maxNrNodes)(CanonicalForm(1))

    def buildTree(nodeIndex: Int, leftEnd: Int, rightEnd: Int): Unit = {
      if (leftEnd == rightEnd) segmentTree(nodeIndex) = canonicalForms(array(leftEnd) - 1)
      else {
        val mid: Int = (leftEnd + rightEnd) / 2
        val leftChildIx: Int = 2 * nodeIndex + 1
        val rightChildIx: Int = leftChildIx + 1
        buildTree(leftChildIx, leftEnd, mid)
        buildTree(rightChildIx, mid + 1, rightEnd)
        segmentTree(nodeIndex) = CanonicalForm.leastCommonMultiple(segmentTree(leftChildIx), segmentTree(rightChildIx))
      }
    }

    buildTree(nodeIndex = 0, leftEnd = 0, rightEnd = n - 1)
    segmentTree
  }

  private def getSegmentMinimumMultiple(segmentTree: Array[CanonicalForm], n: Int, i: Int, j: Int): CanonicalForm = {
    def loop(nodeIndex: Int, leftEnd: Int, rightEnd: Int): CanonicalForm = {
      if (i <= leftEnd && j >= rightEnd) segmentTree(nodeIndex)
      else if (i > rightEnd || j < leftEnd) CanonicalForm(1)
      else {
        val mid: Int = (leftEnd + rightEnd) / 2
        val leftChildIx: Int = 2 * nodeIndex + 1
        val rightChildIx: Int = leftChildIx + 1
        CanonicalForm.leastCommonMultiple(loop(leftChildIx, leftEnd, mid), loop(rightChildIx, mid + 1, rightEnd))
      }
    }

    loop(0, 0, n - 1)
  }

  private def updateSegmentTree(segmentTree: Array[CanonicalForm], n: Int, ix: Int, m: CanonicalForm): Unit = {
    def update(nodeIndex: Int, leftEnd: Int, rightEnd: Int): Unit = {
      if (leftEnd == rightEnd) segmentTree(nodeIndex) = segmentTree(nodeIndex) * m
      else {
        val mid: Int = (leftEnd + rightEnd) / 2
        val leftChildIx: Int = 2 * nodeIndex + 1
        val righChildIx: Int = leftChildIx + 1
        if (leftEnd <= ix && ix <= mid) update(leftChildIx, leftEnd, mid)
        else update(righChildIx, mid + 1, rightEnd)
        segmentTree(nodeIndex) = CanonicalForm.leastCommonMultiple(segmentTree(leftChildIx), segmentTree(righChildIx))
      }
    }

    update(0, 0, n - 1)
  }

  def calcMinimumMultiples(array: Vector[Int], n: Int, operations: List[Operation]): List[Int] = {
    val canonicalForms: Vector[CanonicalForm] = (1 to UpperLimit).map(CanonicalForm(_)).toVector
    val segmentTree: Array[CanonicalForm] = calcSegmentTree(array, canonicalForms, n)

    @tailrec
    def loop(ops: List[Operation], acc: List[Int]): List[Int] = ops match {
      case Nil => acc.reverse
      case op :: rest => op match {
        case Query(i, j) =>
          val minimumMultipleForm: CanonicalForm = getSegmentMinimumMultiple(segmentTree, n, i, j)
          loop(rest, minimumMultipleForm.asInt :: acc)
        case Update(ix, m) =>
          val formOfM: CanonicalForm = canonicalForms(m - 1)
          updateSegmentTree(segmentTree, n, ix, formOfM)
          loop(rest, acc)
      }
    }

    loop(operations, Nil)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val array: Vector[Int] = convertToIntVector(reader.next())
    val nrOperations: Int = reader.next().toInt
    val operations: List[Operation] = reader.take(nrOperations).map(readOperation).toList
    val result: List[Int] = calcMinimumMultiples(array, n, operations)
    result.foreach(println)
  }
}
