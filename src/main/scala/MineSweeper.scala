import scala.util.{Try, Success, Failure}
import scalaz._

case class Cell(hasMine: Boolean, isOpen: Boolean = false, isFlagged: Boolean = false) {
 object SteppedOnAMineException extends Exception("You stepped on a Mine!")

  override def toString: String = this match {
    case _ if `isOpen` => "o"
    case _ if `isFlagged` => "f"
    case _ => "x"
  }

  def flag: Try[Cell] = this match {
    case _ => Success(new Cell(hasMine, isOpen, true))
  }

  def open: Try[Cell] = this match {
    case _ if `hasMine` => Failure(SteppedOnAMineException)
    case _ => Success(new Cell(hasMine, true, isFlagged))
  }

  def isComplete: Boolean = this match {
    case _ if `hasMine` && `isFlagged` => true 
    case _ if !`hasMine` && `isOpen` => true 
    case _ => false
  }
}

class MineField(cells: List[List[Cell]]) {
  override def toString: String = {
    cells.map(_.mkString("")).mkString("\n")
  }

  def action(fn: Cell => Try[Cell], x: Int, y: Int): MineField = {
    val result = fn(cells(x)(y)) match {
      case Success(x) => x
      case Failure(e) => throw e 
    }
    new MineField(cells.updated(x, cells(x).updated(y, result)))
  }

  def isComplete: Boolean = {
    cells.flatten.map(_.isComplete).reduceLeft(_ && _)
  }
}
