import scala.util.{Try, Success, Failure}
import scalaz._

object Collection {
  def map[A, B](list: List[A], function: A => B): List[B] = list match {
    case Nil => List()
    case head::remaining => List(function(head)) ++ map(remaining, function)
  }

  def filter[A](list: List[A], function: A => Boolean): List[A] = list match {
    case Nil => List()
    case head::remaining if function(head) => List(head) ++ filter(remaining, function)
    case head::remaining => filter(remaining, function)
  }

  def reduce[A](list: List[A], seed: A, function: (A, A) => A): Option[A] = list match {
    case Nil => None
    case head::Nil => Some(function(seed, head))
    case head::remaining => reduce(remaining, function(seed, head), function)
  }

  case class InvalidInputException(message: String) extends Exception(message)
  def take[A](list: List[A], number: Int): Try[List[A]] = (list, number) match {
    case (_, 0) => Success(List())
    case (head::remaining, index) => take(remaining, index - 1) match {
                                        case Failure(e) => Failure(e)
                                        case Success(result) => Success(List(head) ++ result)
                                     }
    case _ => Failure(new InvalidInputException("Invalid input"))
  }

  def drop[A](list: List[A], number: Int): Try[List[A]] = (list, number) match {
    case (_, 0) => Success(list)
    case (head::remaining, index) => drop(remaining, index - 1)
    case _ => Failure(new InvalidInputException("Invalid input"))
  }

  def takeWhile[A](list: List[A], function: A => Boolean): List[A] = list match {
    case head::remaining if function(head) => List(head) ++ takeWhile(remaining, function)
    case _ => List()
  }

  def dropWhile[A](list: List[A], function: A => Boolean): List[A] = list match {
    case head::remaining if function(head) => dropWhile(remaining, function)
    case _ => list
  }

  def zip[A, B, C](function: (A, B) => C, list_a: List[A], list_b: List[B]): List[C] = (list_a, list_b) match {
    case (head_a::remaining_a, head_b::remaining_b) => List(function(head_a, head_b)) ++ zip(function, remaining_a, remaining_b)
    case _ => List()
  }  
}
