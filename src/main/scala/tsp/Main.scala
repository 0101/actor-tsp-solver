package tsp

import akka.util.Duration
import akka.util.duration._


case class TspResult(solution: State, duration: Duration)


object TSP {
  
  def using(solver: Solver)(graph: Graph) = {
    val start = System.currentTimeMillis()
    val task = Task(graph, State.initial(graph.size), best=State.worst)
    
    val solution = solver solve task
        
    TspResult(solution, duration=(System.currentTimeMillis() - start).millis)
  }
      
}


object Main extends App {
     
  if (args.length < 1) 
    println("Usage: tsp path/to/graph.txt")
    
  else {
    val graph = FileReader loadGraph args(0)
    
    val result1 = (TSP using SerialSolver)(graph)
    println("Serial solver", result1)
    
    val actorSolver = new ActorSolver(using=SerialSolver,
        initialExpandRatio=10)	  
    val result2 = (TSP using actorSolver)(graph)    
    println(
        "Actor solver with %s workers" format actorSolver.workerCount,
        result2, 
        "%s times faster" format result1.duration / result2.duration)
  }
  
}