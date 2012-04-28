package tsp

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.Stack


object SerialSolver extends Solver {
   
  def solve(graph: Graph, initialState: State, best: State): State = {

    // stupid static typing
    val None = -1

    val stack = Stack[State]()

    stack.push(initialState)

    var localBest = best

    def expand(state: State) {

      if (!(state betterThan localBest)) return

      if (state.remaining.isEmpty) {
        // last move
        val src = state.path.last
        val dest = state.path(0)
        val cost = state.cost + (graph distance src -> dest)
        val finalState = State(state.path, State.emptySeq, cost)
        if (finalState betterThan localBest)
          localBest = finalState
      }

      val firstMove = state.path.isEmpty

      val src = if (firstMove) None else state.path.last

      for (node <- state.remaining.sorted)
        stack.push(State(
          state.path :+ node,
          state.remaining filter (_ != node),
          state.cost + (if (firstMove) 0 else graph distance src -> node)))
    }

    while (!stack.isEmpty) expand(stack.pop())

    localBest

  }
}
