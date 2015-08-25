package week5

object listFun {
  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())((t: T, acc: List[U]) => t match {
      case Nil => acc
      case t => f(t) :: acc
    })                                            //> mapFun: [T, U](xs: List[T], f: T => U)List[U]
    
  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)((t: T, acc: Int) => t match {
      case Nil => acc
      case t => 1 + acc
    })                                            //> lengthFun: [T](xs: List[T])Int

  mapFun(List[Int](1, 2, 3), (x: Int) => "Number " + x.toString)
                                                  //> res0: List[String] = List(Number 1, Number 2, Number 3)
  lengthFun(List[Int](1, 2, 3, 4, 6))             //> res1: Int = 5

}