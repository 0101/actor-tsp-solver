package tsp

trait Solver {

  def solve(graph: Graph, initialState: State, best: State): State
  
}