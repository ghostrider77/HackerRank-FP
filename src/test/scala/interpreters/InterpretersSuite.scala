package interpreters

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class InterpretersSuite extends AnyFreeSpec with Matchers {

  "BrainfuckInterpreter" - {
    import BrainfuckInterpreter.{calcBracketPairIndices, readProgram, Result, runInterpreter, Instruction}

    "should print the result of a valid Brainfuck code" - {
      "test case 1" in {
        val nrLines: Int = 20
        val input: List[Char] = Nil
        val code: Iterator[String] =
          Iterator(
            "+++++ +++++             initialize counter (cell #0) to 10",
            "[                       use loop to set the next four cells to 70/100/30/10",
            "> +++++ ++              add  7 to cell #1",
            "> +++++ +++++           add 10 to cell #2",
            "> +++                   add  3 to cell #3",
            "> +                     add  1 to cell #4",
            "<<<< -                  decrement counter (cell #0)",
            "]",
            "> ++ .                  print 'H'",
            "> + .                   print 'e'",
            "+++++ ++ .              print 'l'",
            ".                       print 'l'",
            "+++ .                   print 'o'",
            "> ++ .                  print ' '",
            "<< +++++ +++++ +++++ .  print 'W'",
            "> .                     print 'o'",
            "+++ .                   print 'r'",
            "----- - .               print 'l'",
            "----- --- .             print 'd'",
            "> + .                   print '!'"
          )
        val program: List[Instruction] = readProgram(code, nrLines)
        val bracketPairPositions: Map[Int, Int] = calcBracketPairIndices(program)
        val result: Result = runInterpreter(program.toVector, input, bracketPairPositions)
        result.toString shouldEqual "Hello World!"
      }

      "test case 2" in {
        val nrLines: Int = 6
        val input: List[Char] = "abcxyz".toList
        val code: Iterator[String] =
          Iterator(
            ",+. This program will 6 characters",
            ",+. For first 3 characters it will",
            ",+. print its successor",
            ",-. For last 3 characters it will",
            ",-. print its predecessor",
            ",-."
        )
        val program: List[Instruction] = readProgram(code, nrLines)
        val bracketPairPositions: Map[Int, Int] = calcBracketPairIndices(program)
        val result: Result = runInterpreter(program.toVector, input, bracketPairPositions)
        result.toString shouldEqual "bcdwxy"
      }

      "test case 3" in {
        val nrLines: Int = 10
        val input: List[Char] = "pm".toList
        val code: Iterator[String] =
          Iterator(
            "++",
            "[           loop will execute only 2 time",
            "    >",
            "    ,       reads a value",
            "    +++     increase by 3",
            "    .       print it",
            "    <",
            "    -",
            "]",
            "+[]"
          )
        val program: List[Instruction] = readProgram(code, nrLines)
        val bracketPairPositions: Map[Int, Int] = calcBracketPairIndices(program)
        val result: Result = runInterpreter(program.toVector, input, bracketPairPositions)
        result.toString shouldEqual "sp\nPROCESS TIME OUT. KILLED!!!"
      }
    }
  }
}
