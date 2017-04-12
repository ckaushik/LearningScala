import org.scalatest._

class HangmanSpec extends FlatSpec with Matchers {
  "Hangman" should "be initialized with word and number of tries" in {
    var hangman = new Hangman("hello", List[Option[Char]](None, None, Some('e'), None, None, Some('o')), 5)
  }
}
