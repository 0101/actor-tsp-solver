package tsp

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.Stack


object SerialSolver extends Solver {
   
  def solve(task: Task) = {
    
    val stack = Stack[State](task.initialState)

    var localBest = task.best

    while (!stack.isEmpty)
      localBest = Solver.expandStep(state=stack.pop(), task.graph, localBest, 
          store=(stack.push(_)))

    localBest
  }
}
