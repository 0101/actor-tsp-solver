package tsp

import scala.collection.mutable.Queue

import akka.actor._
import akka.dispatch.Await
import akka.pattern.ask
import akka.routing.{RoundRobinRouter, Broadcast}
import akka.util.Duration
import akka.util.duration._

sealed trait TspMessage
case object Start extends TspMessage
case class Work(state: State) extends TspMessage
case class Best(state: State) extends TspMessage
case class Result(result: State) extends TspMessage

    
class Worker(solver: Solver, graph: Graph) extends Actor {

  var localBest = State.worst
  
  def receive = {  
    
    case Work(state) => sender ! Result(process(state))
      
    case Best(state) => localBest = state
      
    case x => println("worker received unknown message:", x)     
  }  
  
  def process(state: State) = {
    localBest = solver solve Task(graph, initialState=state, best=localBest)
    localBest
  }  
}

    
class Master(
    solver: Solver, 
    task: Task, 
    workerCount: Int, 
    initialExpandRatio: Int) extends Actor {
  
  val Task(graph, initialState, best) = task
  
  var globalBest = best

  var resultsReceived = 0
  var expectedResultCount = 0
  
  val workers = context.actorOf(
      Props(new Worker(solver, graph))
      	.withRouter(RoundRobinRouter(workerCount)),
      name="workerRouter")
  
  val queue = Queue[State](initialState)
  
  var customer: ActorRef = null

  def initialExpand() {	  
	  val initialSize = workerCount * initialExpandRatio
	  while (queue.size < initialSize && !queue.isEmpty)
	    globalBest = Solver.expandStep(state=queue.dequeue(), 
	        graph, globalBest, store=(queue.enqueue(_)))
  }

  def startCalculation() = {    
    initialExpand()    
    if (queue.isEmpty) 
      deliverResultsAndShutdown()       
	else {	         
		expectedResultCount = queue.size		
		queue foreach (workers ! Work(_))    
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
      customer = sender
      startCalculation()
      
    case Result(state) => 
      resultsReceived += 1
      if (state betterThan globalBest) {
        globalBest = state
        if (!finished) 
          broadcastBest(state)
      }
      if (finished) 
        deliverResultsAndShutdown()
              
    case x => println("master received unknown message:", x)
  }        
}


class ActorSolver(
    using: Solver, 
    val workerCount: Int = 4,
    initialExpandRatio: Int = 50, 
    timeLimit: Duration = 600 seconds) extends Solver {

  def solve(task: Task) = {
    
    val system = ActorSystem("TspSystem")
    
    val master = system.actorOf(
        Props(new Master(using, task, workerCount, initialExpandRatio)), 
        name="master")
    
    val future = (master ? Start)(timeLimit)
    
    Await.result(future, timeLimit).asInstanceOf[State]        
  }  
}