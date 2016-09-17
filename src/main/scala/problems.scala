object Problem {
  def last[A](ls:List[A]):A = ls.last

  def lastRecursion[A](ls:List[A]):A = ls match {
    case h::Nil  => h
    case _::tail => last(tail)
    case Nil     => throw new NoSuchElementException
  }

  def secondToLast[A](ls:List[A]):A = ls.init.last

  def secondToLastRecursion[A](ls:List[A]):A = ls match {
    case h :: _ :: Nil => h
    case _ :: tail     => secondToLastRecursion(tail)
    case Nil           => throw new NoSuchElementException
  }

  def kth[A](n:Int, ls:List[A]):A =
    if (n>=0) ls(n) else throw new IllegalArgumentException

  def kthRecursion[A](n:Int, ls:List[A]):A = (n, ls) match {
    case (0, h :: _)    => h
    case (a, _ :: tail) => kthRecursion(a-1, tail)
    case (a, Nil)       => throw new NoSuchElementException
  }

  def length[A](ls:List[A]):Int = ls.length

  def lengthRecursion[A](ls:List[A], count:Int = 0):Int = (ls, count) match {
    case (h::Nil, a)  => a+1
    case (_::tail, a) => lengthRecursion(tail, a+1)
    case (Nil, a)     => throw new NoSuchElementException
  }

  def lengthFunctional[A](ls:List[A]):Int =
    ls.foldLeft(0)((c, _) => c + 1)

  def reverse[A](ls:List[A]):List[A] = ls.reverse

  def reverseRecursion[A](ls: List[A]): List[A] = {
    def reverseR(result: List[A], curList: List[A]): List[A] = curList match {
      case Nil       => result
      case h :: tail => reverseR(h :: result, tail)
    }
    reverseR(Nil, ls)
  }

  def reverseFunctional[A](ls:List[A]):List[A] = {
    ls.foldLeft(List.empty[A])((acc,h) => h::acc)
  }

  def palindrome(ls:List[_]):Boolean = ls.reverse == ls

  def flatten(ls:List[_]):List[Any] = ls flatMap {
    case m:List[_] => flatten(m)
    case n         => List(n)
  }

  def compress[M](ls:List[M]):List[M] = {
    ls.foldLeft(List.empty[M]){
      case (List(), y) => List(y)
      case (ls, y) => if (ls.last == y) ls else ls:::List(y)
    }
  }
    
}
