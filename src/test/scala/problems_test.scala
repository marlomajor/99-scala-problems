import org.scalatest.{Matchers, FunSuite}

class ProblemTest extends FunSuite with Matchers {

  test("find last element of list with recursion") {
    Problem.lastRecursion(List(1,2,3)) should be (3)
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

  test("find number of elements with recursion") {
    Problem.lengthRecursion(List(1,1,2,3,3,4)) should be (6)
  }

  test("find number of elements purely functionally") {
    Problem.lengthFunctional(List(1,1,2,3,4)) should be (5)
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

  test("flatten list") {
    Problem.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) should be
      (List(1, 1, 2, 3, 5, 8))
  }

  test("eliminate consecutive duplicates of list elements") {
    Problem.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be
      (List('a, 'b, 'c, 'a, 'd, 'e))
  }

  test("Pack consecutive duplicates of list elements into sublists") {
    Problem.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be
      (List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }

  test("Consecutive duplicates of elements are encoded as tuples (N, E)") {
    Problem.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be
      List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  }

  test("duplicate elements of list") {
    Problem.duplicate(List('a, 'b, 'c, 'c, 'd)) should be (List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

  test("duplicate elements of list n times") {
    Problem.duplicateN(3, List('a, 'b, 'c, 'c, 'd)) should be (List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }
}
