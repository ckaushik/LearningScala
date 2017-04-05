import org.scalatest._
import scala.util.{Try, Success, Failure}

class CollectionUtilSpec extends FlatSpec with Matchers {
  "An Map on an empty list" should "return an empty list" in {
    val emptyList = List()
    Collection.map(emptyList, (x: Int) => x + 1) should be (emptyList)
  }

  "A Map on a list with single element" should "return output of the given function" in {
    val singleElementList = List[Int](1)
    Collection.map(singleElementList, (x: Int) => x + 1) should be (List[Int](2))
  }

  "A Map on a list of elements" should "return list of outputs of the given function with respective elements" in {
    val list = List[Int](1, 4)
    Collection.map(list, (x: Int) => x + 1) should be (List[Int](2, 5))
  }

  "A Map" should "not alter the input list" in {
    val list = List[Int](1, 4)
    Collection.map(list, (x: Int) => x + 1)
    list should be (List[Int](1, 4))
  }

  "A Filter on an empty list" should "return empty" in {
    val emptyList = List()
    Collection.filter(emptyList, (x: Int) => x > 100) should be (emptyList)
  }

  "A Filter on a list with single element" should "return empty list if the element doesn't pass the filter" in {
    val singleElementList = List[Int](1)
    Collection.filter(singleElementList, (x: Int) => x > 100) should be (List())
  }

  "A Filter on a list with single element" should "return a list with that element if it passes the filter" in {
    val singleElementList = List[Int](101)
    Collection.filter(singleElementList, (x: Int) => x > 100) should be (List[Int](101))
  }

  "A Reduce on an empty list" should "return nothing" in {
    val emptyList = List()
    Collection.reduce(emptyList, 10, (x: Int, y: Int) => x + y) should be (None)
  }

  "A Reduce on a single element list" should "return result of function with seed and the element" in {
    val singleElementList = List(1)
    Collection.reduce(singleElementList, 10, (x: Int, y: Int) => x - y) should be (Some(9))
  }

  "A Reduce on a list" should "return result of function with seed and the elements in the list" in {
    val list = List(1, 2)
    Collection.reduce(list, 10, (x: Int, y: Int) => x - y) should be (Some(7))
  }

  "Take on a empty list" should "return nothing" in {
    val emptyList = List()
    val badInput = Collection.take(emptyList, 5)
    badInput.failed.get shouldBe a [Collection.InvalidInputException]

    val validInput = Collection.take(emptyList, 0)
    validInput.get should be (List())
  }

  "Take with an invalid number" should "return nothing" in {
    val list = List(1, 2)
    val badInput = Collection.take(list, -2)
    badInput.failed.get shouldBe a [Collection.InvalidInputException]
  }

  "Take with one" should "return first element" in {
    val list = List(1, 2)
    Collection.take(list, 1).get should be (List(1))
  }

  "TakeWhile on an empty list" should "return nothing" in {
    val emptyList = List()
    Collection.takeWhile(emptyList, (x: Int) => x > 0) should be (List())
  }

  "TakeWhile" should "return first set of elements that match the condition" in {
    val list = List(1, 2, 3, 1)
    Collection.takeWhile(list, (x: Int) => x < 3) should be (List(1, 2))
  }

  "TakeWhile" should "return entire list if all the elements match the function" in {
    val list = List(1, 2, 3)
    Collection.takeWhile(list, (x: Int) => x < 100) should be (list)
  }

  "Drop on a empty list" should "return nothing" in {
    val emptyList = List()
    val badInput = Collection.drop(emptyList, 5)
    badInput.failed.get shouldBe a [Collection.InvalidInputException]

    val validInput = Collection.drop(emptyList, 0)
    validInput.get should be (List())
  }

  "Drop with an invalid number" should "return nothing" in {
    val list = List(1, 2)
    val badInput = Collection.drop(list, -2)
    badInput.failed.get shouldBe a [Collection.InvalidInputException]
  }

  "Drop with one" should "return list without first element" in {
    val list = List(1, 2)
    Collection.drop(list, 1).get should be (List(2))
  }

  "DropWhile on an empty list" should "return nothing" in {
    val emptyList = List()
    Collection.dropWhile(emptyList, (x: Int) => x > 0) should be (List())
  }

  "DropWhile" should "return first set of elements that match the condition" in {
    val list = List(1, 2, 3, 2)
    Collection.dropWhile(list, (x: Int) => x < 3) should be (List(3, 2))
  }

  "DropWhile" should "return empty list if all the elements match the function" in {
    val list = List(1, 2, 3)
    Collection.dropWhile(list, (x: Int) => x < 100) should be (List())
  }

  "Zip" should "return empty array when one of the lists is empty" in {
    val list_a = List(1, 2, 3)
    val list_b = List()
    Collection.zip((x: Int, y: Int) => x+y, list_a, list_b) should be (List())
  }

  "Zip" should "return array of results as long as there are elements in either lists" in {
    val list_a = List(1, 2, 3)
    val list_b = List(2, 5)
    Collection.zip((x: Int, y: Int) => x+y, list_a, list_b) should be (List(3, 7))
  }
}
