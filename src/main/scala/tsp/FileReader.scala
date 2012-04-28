package tsp
import scala.io.Source

object FileReader {
  
  def readLines(filename: String): Iterator[String] =
    Source fromFile filename getLines()
  
  def parseNumbers(line: String) = line split "\\s+" map (_.toInt)

  def loadGraph(filename: String): Graph = {
    val lines = this readLines filename
    Graph(
      size = lines.next().toInt,
      distanceMatrix = (lines map (parseNumbers(_).toIndexedSeq)).toIndexedSeq)
  }

}
