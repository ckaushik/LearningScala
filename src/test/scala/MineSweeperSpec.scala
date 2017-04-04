import org.scalatest._

class MineSweeperSpec extends FlatSpec with Matchers {
  "Mine-Cell:: isComplete" should "be true when Mine is Flagged" in {
    new Cell(true, false, true).isComplete should be (true)
  }

  "No-Mine-Cell:: isComplete" should "be true when a cell with no Mine is Open" in {
    new Cell(false, true, false).isComplete should be (true)
  }

  "Cell:: isComplete" should "be false when it is not open" in {
    new Cell(false, false, false).isComplete should be (false)
    new Cell(true, false, false).isComplete should be (false)
  }

  "MineField show" should "print all cells" in {
    val mineField = new MineField(List(List(Cell(false), Cell(false)), List(Cell(false), Cell(false))))
    mineField.toString should be ("xx\nxx")
  }

  "MineField show" should "print empty grid" in {
    val mineField = new MineField(List[List[Cell]]())
    mineField.toString should be ("")
  }

  "MineField" should "be initialized with cells" in {
    val mineField = new MineField(List[List[Cell]]())
  }

  "MineField" should "be initialized with some Mines" in {
    val mineField = new MineField(List(List(Cell(false), Cell(true)), List(Cell(true), Cell(false))))
    mineField.toString should be ("xx\nxx")
  }

  "MineField flag" should "flag the selected cell" in {
    val mineField = new MineField(List(List(Cell(false), Cell(true)), List(Cell(false), Cell(false))))
    mineField.action({c => c.flag}, 1,0).toString should be ("xx\nfx")
  }

  "MineField open on a safe cell" should "open the cell" in {
    val mineField = new MineField(List(List(Cell(false), Cell(true)), List(Cell(false), Cell(false))))
    mineField.action({c => c.open}, 0, 0).toString should be ("ox\nxx")
  }

  "MineField open on a mine cell" should "throw an Exception" in {
    val mineField = new MineField(List(List(Cell(false), Cell(true)), List(Cell(false), Cell(false))))
    an [Exception] should be thrownBy mineField.action({c => c.open}, 0, 1)
  }
}
