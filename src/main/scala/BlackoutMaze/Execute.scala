package BlackoutMaze

/**
 * Created with IntelliJ IDEA.
 * To change this template use File | Settings | File Templates.
 */
object Execute {
  def main(args:Array[String]){
    val startTime = System.currentTimeMillis()
    TraverseMaze.wayOut
    val endTime = System.currentTimeMillis()
    println("*********** Total Time ********************** "+ (endTime - startTime)/(1000 ) + " seconds" )

  }
}
