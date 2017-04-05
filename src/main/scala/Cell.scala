import scala.util.{Try, Success, Failure}

abstract class Cell(state: State = Closed) {
  object SteppedOnAMineException extends Exception("You stepped on a Mine!")
  def flag: Try[Cell]
  def open: Try[Cell]
  def isComplete: Boolean

  override def toString: String = state match {
    case Opened => "o"
    case Flagged => "f"
    case _ => "x"
  }
}

case class SafeCell(state: State = Closed) extends Cell(state){
  override def flag: Try[SafeCell] = {
    Success(SafeCell(Flagged))
  }

  override def open: Try[SafeCell] = {
    Success(SafeCell(Opened))
  }

  def unapply(cell: SafeCell): Option[State] = Some(cell.state)

  override def isComplete: Boolean = state match {
    case Opened => true
    case _ => false
  }
}

case class Mine(state: State = Closed) extends Cell(state){
  override def flag: Try[Mine] = {
    Success(Mine(Flagged))
  }

  override def open: Try[Mine] = {
    Failure(SteppedOnAMineException)
  }
  
  override def isComplete: Boolean = state match {
    case Opened => false 
    case _ => true
  }
}
