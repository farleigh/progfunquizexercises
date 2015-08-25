package week5

object higherOrderListFunctions {
  def squareList1(xs: List[Int]): List[Int] = xs match {
    case Nil => xs
    case y :: ys => y * y :: squareList1(ys)
  }                                               //> squareList1: (xs: List[Int])List[Int]

  squareList1(List(1, 2, 3, 4, 5))                //> res0: List[Int] = List(1, 4, 9, 16, 25)

  def squareList2(xs: List[Int]): List[Int] =
    xs map (x => x * x)                           //> squareList2: (xs: List[Int])List[Int]

  squareList2(List(1, 2, 3, 4, 5))                //> res1: List[Int] = List(1, 4, 9, 16, 25)

  //  pack(List("a", "a", "a", "b", "c", "c", "a"))
  //should give
  //  List(List("a", "a", "a"), List("b"), List("c", "c"), List("a")).
  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (first, rest) = xs span (y => y == x)
      first :: pack(rest)
  }                                               //> pack: [T](xs: List[T])List[List[T]]

  pack(List("a", "a", "a", "b", "c", "c", "a"))   //> res2: List[List[String]] = List(List(a, a, a), List(b), List(c, c), List(a))
                                                  //| 

  def encode[T](xs: List[T]): List[(T, Int)] = {
    val ys = pack(xs)
    ys.map(x => (x.head, x.size))
  }                                               //> encode: [T](xs: List[T])List[(T, Int)]

  encode(List("a", "a", "a", "b", "c", "c", "a")) //> res3: List[(String, Int)] = List((a,3), (b,1), (c,2), (a,1))

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())(???)                 //> mapFun: [T, U](xs: List[T], f: T => U)List[U]

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)(???)                         //> lengthFun: [T](xs: List[T])Int
}