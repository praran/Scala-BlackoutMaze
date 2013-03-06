package BlackoutMaze

/**
 * Created with IntelliJ IDEA.
 * User: u0166888
 * Date: 05/03/13
 * Time: 11:41
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
