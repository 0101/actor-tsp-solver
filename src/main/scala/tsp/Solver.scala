package tsp


case class Task(graph: Graph, initialState: State, best: State)

trait Solver { def solve(task: Task): State }

object Solver {
  
  def expandStep(
      // a State from which to derive possible moves
      state: State,
      
      graph: Graph,
      
      // best known solutions, moves that are already worse than this 
      // will be discarded
      best: State,
      
      // this should store the new States
      store: State => Unit    
  ): State = {
    
	var newBest = best
    
    if (!(state betterThan best)) return newBest
   
    if (state.remaining.isEmpty) {
      // last move
      val src = state.path.last
      val dest = state.path(0)
      val cost = state.cost + (graph distance src -> dest)
      val finalState = State(state.path, State.emptySeq, cost)
      if (finalState betterThan best)
        newBest = finalState
    }
  
    val firstMove = state.path.isEmpty
  
    val src = if (firstMove) -1 else state.path.last
  
    for (node <- state.remaining.sorted)
      store(State(
        state.path :+ node,
        state.remaining filter (_ != node),
        state.cost + (if (firstMove) 0 else graph distance src -> node)))
        
     newBest
  }
}


