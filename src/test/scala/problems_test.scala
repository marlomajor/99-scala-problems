import org.scalatest.{Matchers, FunSuite}

class ProblemTest extends FunSuite with Matchers {
  test("find last element of list") {
    Problem.last(List(1,3,4,5,6,7)) should be (7)
  }

  test("find last element of list with recursion") {
    Problem.lastRecursion(List(1,2,3)) should be (3)
  }

  test("find last but one element") {
    Problem.secondToLast(List(1,2,3,4,5)) should be (4)
  }

  test("find last but one element with recursion") {
    Problem.secondToLastRecursion(List(1,2,3,4,5)) should be (4)
  }

  test("find kth element") {
    Problem.kth(3, List(1,2,3,4,5)) should be (4)
  }

  test("find kth element with recursion") {
    Problem.kthRecursion(4, List(1,3,4,6,7,9)) should be (7)
  }

  test("find number of elements") {
    Problem.length(List(1,2,3,4,5,7)) should be (6)
  }

  test("find number of elements with recursion") {
    Problem.lengthRecursion(List(1,1,2,3,3,4)) should be (6)
  }

  test("find number of elements purely functionally") {
    Problem.lengthFunctional(List(1,1,2,3,4)) should be (5)
  }

  test("reverse list") {
    Problem.reverse(List(1,2,3,4,5)) should be (List(5,4,3,2,1))
  }

  test("reverse list with recursion"){
    Problem.reverseRecursion(List(1,2,3,4,5)) should be (List(5,4,3,2,1))
  }

  test("reverse list functionally") {
    Problem.reverseFunctional(List(1,2,3,4)) should be (List(4,3,2,1))
  }

  test("list is a palindrome") {
    Problem.palindrome(List(1,2,3,2,1)) should be (true)
  }
}
