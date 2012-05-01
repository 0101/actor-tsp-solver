package tsp

import akka.util.Duration
import akka.util.duration._


case class TspResult(solution: State, duration: Duration)


object TSP {
  
  def using(solver: Solver)(graph: Graph) = {
    val start = System.currentTimeMillis()
    val solution = solver.solve(graph, State.initial(graph.size), State.worst)
        
    TspResult(solution, duration=(System.currentTimeMillis() - start).millis)
  }
      
}


object Main {
  
  def main(args: Array[String]) =
    
	if (args.length < 1) 
	  println("Usage: tsp path/to/graph.txt")
	  
	else {
	  val graph = FileReader loadGraph args(0)
	  
	  val result1 = (TSP using SerialSolver)(graph)	  
	  println("Serial solver", result1)
	  
	  val actorSolver = new ActorSolver(using=SerialSolver)	  
	  val result2 = (TSP using actorSolver)(graph)	  
	  println("Actor solver", result2, 
	      "%s times faster".format(result1.duration / result2.duration))
	}
      
}