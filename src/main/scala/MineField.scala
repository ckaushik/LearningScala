import scala.util.{Try, Success, Failure}

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
