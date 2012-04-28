package tsp


case class Graph(size: Int, distanceMatrix: IndexedSeq[IndexedSeq[Int]]) {

  def distance(t: (Int, Int)) = distanceMatrix(t._1)(t._2)

  override def toString = distanceMatrix map (_ mkString " ") mkString "\n"

}