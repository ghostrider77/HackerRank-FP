package recursion

object SierpinskiTriangle {
  def createGrid(rows: Int, cols: Int): Array[Array[Char]] = Array.fill(rows, cols)('_')

  def drawSierpinskiTriangle(n: Int, grid: Array[Array[Char]]): Unit = {
    def fillTriangle(n: Int, topX: Int, topY: Int, height: Int): Unit = {
      if (n == 0) {
        for {
          ix <- 0 until height
          y <- topY - ix to topY + ix
        } grid(topX + ix)(y) = '1'
      }
      else {
        fillTriangle(n - 1, topX, topY, height/2)
        fillTriangle(n - 1, topX + height/2, topY - height/2, height/2)
        fillTriangle(n - 1, topX + height/2, topY + height/2, height/2)
      }
    }

    fillTriangle(n, topX = 0, topY = 31, height = 32)
    grid.foreach(row => println(row.mkString))
  }

  def main(args: Array[String]): Unit = {
    drawSierpinskiTriangle(n = io.Source.stdin.getLines().next().toInt, grid = createGrid(32, 63))
  }
}
