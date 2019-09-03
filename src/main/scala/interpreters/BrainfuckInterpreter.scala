package interpreters

object BrainfuckInterpreter {
  import scala.annotation.tailrec

  sealed trait Instruction
  case object IncrementPointer extends Instruction
  case object DecrementPointer extends Instruction
  case object IncrementByte extends Instruction
  case object DecrementByte extends Instruction
  case object PrintChar extends Instruction
  case object ReadByte extends Instruction
  case object LeftBracket extends Instruction
  case object RightBracket extends Instruction
  case object Comment extends Instruction

  object Instruction {
    def apply(c: Char): Instruction = c match {
      case '>' => IncrementPointer
      case '<' => DecrementPointer
      case '+' => IncrementByte
      case '-' => DecrementByte
      case '.' => PrintChar
      case ',' => ReadByte
      case '[' => LeftBracket
      case ']' => RightBracket
      case _ => Comment
    }
  }

  final case class Result(output: List[Char], isTerminated: Boolean) {
    override def toString: String = {
      val resultString: String = output.mkString
      if (!isTerminated) resultString else resultString + "\n" + "PROCESS TIME OUT. KILLED!!!"
    }
  }

  private val NrOperationsLimit: Int = 100000

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def programLine(line: String): List[Instruction] = line.map(Instruction(_)).filterNot(_ == Comment).toList

  def readProgram(inputStream: Iterator[String], nrLines: Int): List[Instruction] =
    inputStream.take(nrLines).toList.flatMap(programLine)

  def calcBracketPairIndices(program: List[Instruction]): Map[Int, Int] = {
    @tailrec
    def loop(acc: List[(Int, Int)],
             bracketIndexStack: List[Int],
             instructions: List[(Instruction, Int)]): Map[Int, Int] = instructions match {
      case Nil => acc.toMap
      case (instruction, ix) :: rest =>
        if (instruction == LeftBracket) loop(acc, ix :: bracketIndexStack, rest)
        else if (instruction == RightBracket) {
          val jy: Int = bracketIndexStack.head
          loop((ix, jy) :: (jy, ix) :: acc, bracketIndexStack.tail, rest)
        }
        else loop(acc, bracketIndexStack, rest)
    }

    loop(Nil, Nil, program.zipWithIndex)
  }

  def runInterpreter(program: Vector[Instruction], input: List[Char],  bracketPairPositions: Map[Int, Int]): Result = {
    def increment(x: Int): Int = (x + 1) % 256
    def decrement(x: Int): Int = (x - 1 + 256) % 256
    def programLength: Int = program.length

    @tailrec
    def loop(acc: List[Char],
             memory: Vector[Int],
             input: List[Char],
             indexOfMemoryLocation: Int,
             indexOfInstruction: Int,
             nrSteps: Int): Result = {
      if (indexOfInstruction >= programLength) Result(acc.reverse, isTerminated = false)
      else if (nrSteps >= NrOperationsLimit) Result(acc.reverse, isTerminated = true)
      else program(indexOfInstruction) match {
        case IncrementPointer =>
          loop(acc, memory, input, indexOfMemoryLocation + 1, indexOfInstruction + 1, nrSteps + 1)
        case DecrementPointer =>
          loop(acc, memory, input, indexOfMemoryLocation - 1, indexOfInstruction + 1, nrSteps + 1)
        case IncrementByte =>
          val memoryValue: Int = memory(indexOfMemoryLocation)
          val memoryUpdated: Vector[Int] = memory.updated(indexOfMemoryLocation, increment(memoryValue))
          loop(acc, memoryUpdated, input, indexOfMemoryLocation, indexOfInstruction + 1, nrSteps + 1)
        case DecrementByte =>
          val memoryValue: Int = memory(indexOfMemoryLocation)
          val memoryUpdated: Vector[Int] = memory.updated(indexOfMemoryLocation, decrement(memoryValue))
          loop(acc, memoryUpdated, input, indexOfMemoryLocation, indexOfInstruction + 1, nrSteps + 1)
        case PrintChar =>
          val memoryValue: Int = memory(indexOfMemoryLocation)
          loop(memoryValue.toChar :: acc, memory, input, indexOfMemoryLocation, indexOfInstruction + 1, nrSteps + 1)
        case ReadByte =>
          val value: Char = input.head
          val memoryUpdated: Vector[Int] = memory.updated(indexOfMemoryLocation, value.toInt)
          loop(acc, memoryUpdated, input.tail, indexOfMemoryLocation, indexOfInstruction + 1, nrSteps + 1)
        case LeftBracket =>
          if (memory(indexOfMemoryLocation) == 0)
            loop(acc, memory, input, indexOfMemoryLocation, bracketPairPositions(indexOfInstruction), nrSteps + 1)
          else loop(acc, memory, input, indexOfMemoryLocation, indexOfInstruction + 1, nrSteps + 1)
        case RightBracket =>
          if (memory(indexOfMemoryLocation) != 0)
            loop(acc, memory, input, indexOfMemoryLocation, bracketPairPositions(indexOfInstruction), nrSteps + 1)
          else loop(acc, memory, input, indexOfMemoryLocation, indexOfInstruction + 1, nrSteps + 1)
        case _ => loop(acc, memory, input, indexOfMemoryLocation, indexOfInstruction + 1, nrSteps)
      }
    }

    val emptyMemory: Vector[Int] = Vector.fill(5000)(0)
    loop(Nil, emptyMemory, input, 0, 0, 0)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(_, nrProgramLines): List[Int] = convertToIntList(reader.next())
    val input: List[Char] = reader.next().dropRight(1).toList
    val program: List[Instruction] = readProgram(reader, nrProgramLines)
    val bracketPairPositions: Map[Int, Int] = calcBracketPairIndices(program)
    val result: Result = runInterpreter(program.toVector, input, bracketPairPositions)
    println(result)
  }
}
