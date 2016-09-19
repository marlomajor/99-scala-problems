object Problem {

  def lastRecursion[A](ls:List[A]):A = ls match {
    case cup :: Nil => cup
    case _::tail => lastRecursion(tail)
    case Nil     => throw new NoSuchElementException
  }

  def secondToLastRecursion[A](ls:List[A]):A = ls match {
    case h :: _ :: Nil => h
    case _ :: tail     => secondToLastRecursion(tail)
    case Nil           => throw new NoSuchElementException
  }

  def kth[A](n:Int, ls:List[A]):A = {
    if (n>=0) ls(n) else throw new IllegalArgumentException
  }

  def kthRecursion[A](n:Int, ls:List[A]):A = (n, ls) match {
    case (0, h) => h.head
    case (n, _::tail) => nth(n-1, tail)
    case (_, Nil) => throw new NoSuchElementException
  }

  def lengthRecursion[A](ls:List[A], count:Int = 0):Int = (ls, count) match {
    case (h::Nil, a)  => a+1
    case (_::tail, a) => lengthRecursion(tail, a+1)
    case (Nil, a)     => throw new NoSuchElementException
  }

  def lengthFunctional[A](ls:List[A]):Int = {ls.foldLeft(0)((c, _) => c + 1)}

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

  def palindrome[A](l: List[A]):Boolean = l match {
    case Nil => true
    case List(a) => true
    case list => (list.head == list.last && palindrome(list.tail.init))
  }

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

  def nth[A](n:Int, ls:List[A]):A = (n, ls) match {
    case (0, h) => h.head
    case (n, _::tail) => nth(n-1, tail)
    case (_, Nil) => throw new NoSuchElementException
  }

  def pack[A](ls:List[A]):List[List[A]] =
    if (ls.isEmpty) List(List())
    else {
      val (packed, nextList) = ls.span {_ == ls(0)}
      if (nextList == Nil) List(packed) else packed::pack(nextList)
    }
}
