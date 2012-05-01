package tsp

import collection.immutable.IndexedSeq


case class State(
  // sequence of visited nodes
  path: IndexedSeq[Int],

  // nodes not yet visited
  remaining: IndexedSeq[Int],

  // total distance
  cost: Int
) {

  def betterThan(that: State) = this.cost < that.cost

  override def toString = {
    
    val len = path.length
    if (len < 1) 
      "no path, cost: %s" format cost
    else {    
	  val zeroIndex = path indexOf 0
	  val leftOfZero = path((zeroIndex - 1 + len) % len)
	  val rightOfZero = path((zeroIndex + 1 + len) % len)
	  val direction = if (leftOfZero > rightOfZero) 1 else -1
	  
	  val normalizedPath = for ((n, i) <- path.iterator.zipWithIndex) 
	    yield path(((direction * i) + zeroIndex + len) % len) 
	  
	  "cost: %s path: %s".format(cost, normalizedPath mkString " ")
    }
  }

}

object State {
  
  val emptySeq = IndexedSeq()
  
  val worst = State(emptySeq, emptySeq, cost=Int.MaxValue)
  
  def initial(size: Int) = State(
      path=emptySeq,
      remaining=(0 to size - 1).toIndexedSeq, 
      cost=0)
    
}