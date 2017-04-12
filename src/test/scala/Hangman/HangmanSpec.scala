import org.scalatest._

class HangmanSpec extends FlatSpec with Matchers with BeforeAndAfterAll {
  "Hangman" should "be initialized with word and number of tries" in {
    var hangman = Hangman("hello", List[Char](), 5)
  }

  "Hangman guess" should "reduce the number of tries if the char doesn't exist in the word" in {
    var hangman = new Hangman("hello", List[Char](), 5)
    hangman.guess('x') should be (Hangman("hello", List[Char]('x'), 4))
  }

  "Hangman guess" should "not reduce number of tries, if the char is present" in {
    var hangman = new Hangman("hello", List[Char]('x'), 5)
    hangman.guess('h') should be (Hangman("hello", List[Char]('x','h'), 5))
  }

  "Hangman print" should "print all the guessed chars" in {
    var hangman = new Hangman("hello", List[Char]('h'), 5)
    hangman.print should be ("h _ _ _ _")
  }

  "Hangman print" should "not print wrongly guessed chars" in {
    var hangman = new Hangman("hello", List[Char]('l', 'x'), 5)
    hangman.print should be ("_ _ l l _")
  }

  "Hangman show" should "not print wrongly guessed chars" in {
    var hangman = new Hangman("hello", List[Char]('l', 'x'), 5)
    hangman.print should be ("_ _ l l _")
  }
}
