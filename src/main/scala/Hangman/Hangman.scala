case class Hangman(word: String, guessedWords: List[Char], numberOfTries: Int){
  def guess(char: Char): Hangman = {
    var Pattern = s"(.*${char}.*)".r
    word match {
      case Pattern(c) => new Hangman(word, guessedWords ++ List(char), numberOfTries)
      case _ => new Hangman(word, guessedWords ++ List(char), numberOfTries - 1)
    }
  }

  def isComplete: Boolean = {
    word.forall((x) => guessedWords contains x)
  }

  def print: String = {
    word.map((x) => if (guessedWords contains x) x else '_') mkString " "
  }
}
