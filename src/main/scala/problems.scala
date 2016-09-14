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

  def kthElement[A](n:Int, ls:List[A]):A = {
    if (n>=0) ls(n) else throw new IllegalArgumentException
  }

  def kthRecursion[A](n:Int, ls:List[A]):A = (n, ls) match {
    case (0, h :: _)    => h
    case (a, _ :: tail) => kthRecursion(a-1, tail)
    case (a, Nil)       => throw new NoSuchElementException
  }

  def length[A](ls:List[A]):Int = ls.length

}
