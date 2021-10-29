package functional_structures

object MatrixRotation {
  import scala.annotation.tailrec
  import scala.collection.mutable.ListBuffer

  type Matrix = Vector[Vector[Int]]
  type Coord = (Int, Int)

  final case class Layer(flattened: Vector[Int], nrRow: Int, nrCol: Int, topLeft: Coord)

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def readParameters(line: String): (Int, Int, Int) = convertToIntList(line) match {
    case List(nRows, nCols, numberOfRotations) => (nRows, nCols, numberOfRotations)
    case _ => throw new Exception("Unexpected input data format.")
  }

  private def peelMatrix(matrix: Vector[Vector[Int]], nRows: Int, nCols: Int): Vector[Layer] = {
    @tailrec
    def getLayers(acc: List[Layer], layerId: Int, rows: Int, cols: Int): List[Layer] = {
      if (rows == 0 || cols == 0) acc
      else {
        val layerElems = ListBuffer.empty[Int]
        for { jy <- layerId until (nCols - layerId) } layerElems.append(matrix(layerId)(jy))
        for { ix <- (layerId + 1) until (nRows - layerId - 1)} layerElems.append(matrix(ix)(nCols- layerId - 1))
        for { jy <- (nCols - layerId - 1) to layerId by -1 } layerElems.append(matrix(nRows - layerId - 1)(jy))
        for { ix <- (nRows - layerId - 2) to (layerId + 1) by -1} layerElems.append(matrix(ix)(layerId))
        val closedLayer: Vector[Int] = layerElems.result().toVector
        getLayers(Layer(closedLayer, rows, cols, (layerId, layerId)) :: acc, layerId + 1, rows - 2, cols - 2)
      }
    }

    getLayers(Nil, 0, nRows, nCols).toVector
  }

  private def shiftLayers(matrixLayers: Vector[Layer], numberOfRotations: Int): Vector[Layer] = {
    def shiftOneLayer(layer: Layer): Layer = {
      val Layer(flattenedLayer, _, _, _) = layer
      val shiftBy: Int = numberOfRotations % flattenedLayer.length
      val (left, right): (Vector[Int], Vector[Int]) = flattenedLayer.splitAt(shiftBy)
      layer.copy(flattened = right ++ left)
    }
    matrixLayers.map(shiftOneLayer)
  }

  private def reconstructShiftedMatrix(shiftedLayers: Vector[Layer], nRows: Int, nCols: Int): Vector[Vector[Int]] = {
    val matrix: Array[Array[Int]] = Array.fill(nRows, nCols)(0)
    for { Layer(shifted, n, m, (topRowIx, topColIx)) <- shiftedLayers } {
      for { jy <- 0 until m } matrix(topRowIx)(topColIx + jy) = shifted(jy)
      for { ix <- 0 until (n - 2 ) } matrix(topRowIx + ix + 1)(topColIx + m - 1) = shifted(m + ix)
      for { jy <- 0 until m } matrix(topRowIx + n - 1)(topColIx + m - 1 - jy) = shifted(m + n - 2 + jy)
      for { ix <- 0 until (n - 2) } matrix(topRowIx + n - 2 - ix)(topColIx) = shifted(2*m + n - 2 + ix)
    }

    matrix.map(_.toVector).toVector
  }

  def calcRotatedMatrix(matrix: Matrix, nRows: Int, nCols: Int, numberOfRotations: Int): Matrix = {
    val matrixLayers: Vector[Layer] = peelMatrix(matrix, nRows, nCols)
    val shiftedLayers: Vector[Layer] = shiftLayers(matrixLayers, numberOfRotations)
    reconstructShiftedMatrix(shiftedLayers, nRows, nCols)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (nRows, nCols, numberOfRotations): (Int, Int, Int) = readParameters(reader.next())
    val matrix: Vector[Vector[Int]] =
      (for { _ <- 0 until nRows } yield reader.next().split(" ").map(_.toInt).toVector).toVector
    val result: Matrix = calcRotatedMatrix(matrix, nRows, nCols, numberOfRotations)
    result.foreach(row => println(row.mkString(" ")))
  }
}
