import org.scalatest._

class MineSweeperSpec extends FlatSpec with Matchers {
  "Mine-Cell:: isComplete" should "be true when Mine is Flagged" in {
    Mine(Flagged).isComplete should be (true)
  }

  "No-Mine-Cell:: isComplete" should "be true when a cell with no Mine is Open" in {
    SafeCell(Opened).isComplete should be (true)
  }

  "Cell:: isComplete" should "be false when it is not open" in {
    SafeCell(Closed).isComplete should be (false)
  }

  "MineField show" should "print all cells" in {
    val mineField = new MineField(List(List(SafeCell(), SafeCell()), List(SafeCell(), SafeCell())))
    mineField.toString should be ("xx\nxx")
  }

  "MineField show" should "print empty grid" in {
    val mineField = new MineField(List[List[Cell]]())
    mineField.toString should be ("")
  }

  "MineField" should "be initialized with cells" in {
    val mineField = new MineField(List[List[Cell]]())
  }

  "MineField" should "be initialized with some Mine()s" in {
    val mineField = new MineField(List(List(SafeCell(), Mine()), List(Mine(), SafeCell())))
    mineField.toString should be ("xx\nxx")
  }

  "MineField flag" should "flag the selected cell" in {
    val mineField = new MineField(List(List(SafeCell(), Mine()), List(SafeCell(), SafeCell())))
    mineField.action({c => c.flag}, 1,0).toString should be ("xx\nfx")
  }

  "MineField open on a safe cell" should "open the cell" in {
    val mineField = new MineField(List(List(SafeCell(), Mine()), List(SafeCell(), SafeCell())))
    mineField.action({c => c.open}, 0, 0).toString should be ("ox\nxx")
  }

  "MineField open on a mine cell" should "throw an Exception" in {
    val mineField = new MineField(List(List(SafeCell(), Mine()), List(SafeCell(), SafeCell())))
    an [Exception] should be thrownBy mineField.action({c => c.open}, 0, 1)
  }
}
