package recursion

object RecursiveTrees {
  def createBackground(rows: Int, cols: Int): Array[Array[Char]] = Array.fill(rows, cols)('_')

  def drawTree(n: Int, background: Array[Array[Char]]): Unit = {
    def branchTree(n: Int, bottomX: Int, bottomY: Int, halfHeight: Int): Unit = {
      if (n == 1) {
        for { ix <- 0 until halfHeight } background(bottomX - ix)(bottomY) = '1'
        for {
          ix <- 0 until halfHeight
          y <- List(bottomY - ix - 1, bottomY + ix + 1)
        } background(bottomX - halfHeight - ix)(y) = '1'
      }
      else {
        branchTree(n - 1, bottomX - 2*halfHeight, bottomY - halfHeight, halfHeight/2)
        branchTree(n - 1, bottomX - 2*halfHeight, bottomY + halfHeight, halfHeight/2)
      }
    }

    (1 to n).foreach(branchTree(_, bottomX = 62, bottomY = 49, halfHeight = 16))
    background.foreach(row => println(row.mkString))
  }

  def main(args: Array[String]): Unit = {
    drawTree(n = io.Source.stdin.getLines().next().toInt, background = createBackground(63, 100))
  }
}
