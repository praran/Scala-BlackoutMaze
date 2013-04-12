package BlackoutMaze

import dispatch.classic.{:/, Http}
import beans.BeanInfo
import sjson.json.Serializer.SJSON
import collection.mutable

/**
 * Created with IntelliJ IDEA.
 * To change this template use File | Settings | File Templates.
 */


//TODO: Fault Tolerance

@BeanInfo case class Cell(note: String,
                          x: BigDecimal,
                          y: BigDecimal,
                          mazeGuid: String,
                          atEnd: Boolean,
                          previouslyVisited: Boolean,
                          north: String,
                          east: String,
                          south: String,
                          west: String) {
  def this() = this("", 0.0, 0.0, "", false, false, "", "", "", "")
}

@BeanInfo case class currentCellObject(currentCell: Cell) {
  def this() = this(Cell("", 0, 0, "", false, false, "", "", "", ""))
}

object TraverseMaze extends Movements with Transformer with General with decisions {
  var count = 0
  /**
   * My Way out
   */
  def wayOut = {
    // initial config
    val x: Cell = getCellInfo(init)
    // recursive way
    def r(x:Cell):Cell ={
      if (x.atEnd){
        return  x
      }
      count = count + 1
       r(go(x))
    }
    // final results
    println("finally !!! "+r(x))
    // Total moves
    println("Total Moves " + count)
  }

  /**
   * Just go
   * @param cell
   * @return
   */
  def go(cell: Cell): Cell = {
    implicit def stringObtoCell(str: String): Cell = getCellInfo(str)
    val direction = whichDirection(cell)
    println("direction to go " + direction)
    direction match {
      case "NORTH" => return ^(cell.mazeGuid)
      case "SOUTH" => return v(cell.mazeGuid)
      case "EAST" => return >(cell.mazeGuid)
      case "WEST" => return <(cell.mazeGuid)
      case _ => {
        val x = crossRoad.pop()
        return jump(cell.mazeGuid, x._1, x._2)
      }
    }
  }

}

sealed trait decisions extends Helpful {
  // Only side effect
  val crossRoad: mutable.Stack[(BigDecimal, BigDecimal)] = new mutable.Stack[(BigDecimal, BigDecimal)]()

  def shouldContinue(cell: Cell): Boolean = {
    !isEnd(cell)
  }

  /**
   * Decides which direction to take
   * @param c
   * @return
   */
  def whichDirection(c: Cell): String = {
    if (isCrossRoad(c)) crossRoad.push((c.x, c.y))
    if (isUnExplored(c, "NORTH")) {
      return "NORTH"
    } else if (isUnExplored(c, "SOUTH")) {
      return "SOUTH"
    } else if (isUnExplored(c, "EAST")) {
      return "EAST"
    } else if (isUnExplored(c, "WEST")) {
      return "WEST"
    }

    return "";
  }

  /**
   * Am I at crossroads
   * @param cell
   * @return
   */
  def isCrossRoad(cell: Cell): Boolean = {
    return (getTraversableRoutes(cell).size > 1)
  }

}


sealed trait Helpful {

  def isEnd(cell: Cell): Boolean = {
    return cell.atEnd
  }

  def isDeadEnd(cell: Cell): Boolean = {
    return (cell.north.equalsIgnoreCase("BLOCKED")
      && cell.south.equalsIgnoreCase("BLOCKED")
      && cell.east.equalsIgnoreCase("BLOCKED")
      && cell.west.equalsIgnoreCase("BLOCKED"))
  }

  def isBlocked(cell: Cell, direction: String): Boolean = {
    direction.toUpperCase match {
      case "NORTH" => cell.north.equalsIgnoreCase("BLOCKED") //cell.north.equalsIgnoreCase("UNEXPLORED")
      case "SOUTH" => cell.south.equalsIgnoreCase("BLOCKED")
      case "EAST" => cell.east.equalsIgnoreCase("BLOCKED")
      case "WEST" => cell.west.equalsIgnoreCase("BLOCKED")
      case _ => false
    }
  }

  def isUnExplored(cell: Cell, direction: String): Boolean = {
    direction.toUpperCase match {
      case "NORTH" => cell.north.equalsIgnoreCase("UNEXPLORED") //cell.north.equalsIgnoreCase("UNEXPLORED")
      case "SOUTH" => cell.south.equalsIgnoreCase("UNEXPLORED")
      case "EAST" => cell.east.equalsIgnoreCase("UNEXPLORED")
      case "WEST" => cell.west.equalsIgnoreCase("UNEXPLORED")
      case _ => false
    }
  }

  def isVisited(cell: Cell, direction: String): Boolean = {
    direction.toUpperCase match {
      case "NORTH" => cell.north.equalsIgnoreCase("VISITED")
      case "SOUTH" => cell.south.equalsIgnoreCase("VISITED")
      case "EAST" => cell.east.equalsIgnoreCase("VISITED")
      case "WEST" => cell.west.equalsIgnoreCase("VISITED")
      case _ => false
    }
  }

  def isTraversable(cell: Cell, direction: String): Boolean = {
    direction.toUpperCase match {
      case "NORTH" => !cell.north.equalsIgnoreCase("BLOCKED")
      case "SOUTH" => !cell.south.equalsIgnoreCase("BLOCKED")
      case "EAST" => !cell.east.equalsIgnoreCase("BLOCKED")
      case "WEST" => !cell.west.equalsIgnoreCase("BLOCKED")
      case _ => false
    }
  }

  def getTraversableRoutes(cell: Cell): List[String] = {
    return List(cell.north.toUpperCase, cell.south.toUpperCase, cell.east.toUpperCase, cell.west.toUpperCase).filter(_.equalsIgnoreCase("UNEXPLORED"))
  }
}

/**
 * Transformer to convert JSON to Cell Object
 */
sealed trait Transformer {
  def getCellInfo(str: String): currentCellObject = {
   return SJSON.in[currentCellObject](str)
  }

}

/**
 * Movement in specific direction
 */
sealed trait Movements extends Actions {
  def >(mazeGuid: String): String = {
    return move(mazeGuid, "EAST")
  }

  def <(mazeGuid: String): String = {
    return move(mazeGuid, "WEST")
  }

  def ^(mazeGuid: String): String = {
    return move(mazeGuid, "NORTH")
  }

  def v(mazeGuid: String): String = {
    return move(mazeGuid, "SOUTH")
  }
}

/**
 * All available actions init, move, jump
 */
sealed trait Actions {
  private val HOSTNAME: String = "epdeveloperchallenge.com"

  def init: String = {
    val http = new Http()
    val res = http(:/(HOSTNAME).POST / ("api/init") <:< Map("contentType" -> "application/x-www-form-urlencoded") >~ {
      _.getLines().mkString
    })
    return res
  }

  def move(mazeGuid: String, direction: String): String = {
    val http = new Http()
    val res = http(:/(HOSTNAME).POST / ("api/move") <:< Map("contentType" -> "application/x-www-form-urlencoded") <<? Map("mazeGuid" -> mazeGuid, "direction" -> direction) >~ {
      _.getLines().mkString
    })
    return res
  }

  def jump(mazeGuid: String, x: BigDecimal, y: BigDecimal): String = {
    val http = new Http()
    val res = http(:/(HOSTNAME).POST / ("api/jump") <:< Map("contentType" -> "application/x-www-form-urlencoded") <<? Map("mazeGuid" -> mazeGuid, "x" -> x.toString(), "y" -> y.toString()) >~ {
      _.getLines().mkString
    })
    return res
  }

}

sealed trait General {
  implicit def currentCelltoCell(co: currentCellObject): Cell = co.currentCell
}
