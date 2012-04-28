package tsp


object TSP {
  
  def using(solver: Solver)(graph: Graph): State =
    solver.solve(graph, State.initial(graph.size), State.worst)
      
}


object Main {
  
  def main(args: Array[String]) =
	if (args.length < 1) 
	  println("Usage: tsp path/to/graph.txt")
	else {
	  val graph = FileReader loadGraph args(0)
	  val result = (TSP using SerialSolver)(graph)
	  
	  println(result)
	}
      
}