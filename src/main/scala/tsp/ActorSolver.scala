package tsp

import scala.collection.mutable.Queue

import akka.actor._
import akka.routing.{RoundRobinRouter, Broadcast}
import akka.dispatch.Await
import akka.util.Duration
import akka.util.duration._
import akka.pattern.ask

sealed trait TspMessage

case object Start extends TspMessage

case class Work(state: State) extends TspMessage
case class Best(state: State) extends TspMessage
case class Result(result: State) extends TspMessage

    
class Worker(solver: Solver, graph: Graph) extends Actor {

  var localBest = State.worst
  
  def receive = {
    
    case Work(state) => 
      println("worker received work", state)
      sender ! Result(process(state))
      
    case Best(state) => 
      println("worker received best", state)
      localBest = state
      
    case _ => println("worker received unknown message")
    
  }
  
  def process(state: State) = {
    localBest = solver.solve(graph, initialState=state, best=localBest)
    localBest
  }
  
}

    
class Master(
    solver: Solver, 
    graph: Graph, initialState: State, best: State,
    workerCount: Int, 
    initialExpandRatio: Int) extends Actor {
  
  var globalBest = best

  var resultsReceived = 0
  var expectedResultCount = 0
  
  val workers = context.actorOf(
      Props(new Worker(solver, graph))
      	.withRouter(RoundRobinRouter(workerCount)),
      name="workerRouter")
  
  val queue = Queue[State]()
  
  var customer: ActorRef = null

  def initialExpand() {
	  queue.enqueue(initialState)
	  
	  val initialExpand = workerCount * initialExpandRatio
	  while (queue.size < initialExpand && !queue.isEmpty)
	    expandStep(queue.dequeue())
  }
  
  def expandStep(state: State) {
    
	// TODO: DRY with serial solver
    
    if (!(state betterThan globalBest)) return
   
    if (state.remaining.isEmpty) {
      // last move
      val src = state.path.last
      val dest = state.path(0)
      val cost = state.cost + (graph distance src -> dest)
      val finalState = State(state.path, State.emptySeq, cost)
      if (finalState betterThan globalBest)
        globalBest = finalState
    }
  
    val firstMove = state.path.isEmpty
  
    val src = if (firstMove) -1 else state.path.last
  
    for (node <- state.remaining.sorted)
      queue.enqueue(State(
        state.path :+ node,
        state.remaining filter (_ != node),
        state.cost + (if (firstMove) 0 else graph distance src -> node)))
  }

  def startCalculation() = {
    
    initialExpand()
    
    if (queue.isEmpty) deliverResultsAndShutdown()    	    
	else {	         
		expectedResultCount = queue.size		
		queue map (workers ! Work(_))    
	}    
  }
  
  def deliverResultsAndShutdown() = {
    customer ! globalBest 
    context.system.shutdown()
  }
    
  def broadcastBest(state: State) = workers ! Broadcast(Best(state))
  
  def finished = resultsReceived >= expectedResultCount
  
  def receive = {
    
    case Start => 
      println("master received start")
      customer = sender
      startCalculation()
      
    case Result(state) => 
      println("master received result", state)
      resultsReceived += 1
      if (state betterThan globalBest) {
        globalBest = state
        if (!finished) 
          broadcastBest(state)
      }
      if (finished) 
        deliverResultsAndShutdown()
              
    case _ => println("master received unknown message")
  }
        
}


class ActorSolver(using: Solver, workerCount: Int = 4,
    initialExpandRatio: Int = 50) extends Solver() {

  def solve(graph: Graph, initialState: State, best: State) = {
    
    val system = ActorSystem("TspSystem")
    
    val master = system.actorOf(Props(
        new Master(using, graph, initialState, best, 
            workerCount, initialExpandRatio)), name="master")
    
    val future = (master ? Start)(600 seconds)
    
    Await.result(future, 600 seconds).asInstanceOf[State]    
    
  }
  
}