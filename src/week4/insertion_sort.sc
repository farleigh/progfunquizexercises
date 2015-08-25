package week4

object insertion_sort {
  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => { if (x <= xs.head) x :: xs else y :: insert(x, ys) }
  }                                               //> insert: (x: Int, xs: List[Int])List[Int]

  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => List()
    case y :: ys => y :: init(ys)
  }                                               //> init: [T](xs: List[T])List[T]

  init(1 :: 2 :: 3 :: 4 :: 5 :: Nil)              //> res0: List[Int] = List(1, 2, 3, 4)

  def removeAt[T](n: Int, xs: List[T]): List[T] = xs match {
    case List() => List()
    case x :: xs => if (n == 1) xs else x :: removeAt(n - 1, xs)
  }                                               //> removeAt: [T](n: Int, xs: List[T])List[T]

  removeAt(2, 1 :: 2 :: 3 :: 4 :: Nil)            //> res1: List[Int] = List(1, 3, 4)
  removeAt(1, 1 :: 2 :: 3 :: Nil)                 //> res2: List[Int] = List(2, 3)
  removeAt(99, 1 :: Nil)                          //> res3: List[Int] = List(1)

  def flatten(xs: List[Any]): List[Any] = xs match {
    case List() => List()
    case (x: List[Any]) :: ys => flatten(x) ::: flatten(ys)
    case x :: xs => x :: flatten(xs)
  }                                               //> flatten: (xs: List[Any])List[Any]

  flatten(List(List(1, 1), 2, List(3, List(5, 8))))
                                                  //> res4: List[Any] = List(1, 1, 2, 3, 5, 8)
}