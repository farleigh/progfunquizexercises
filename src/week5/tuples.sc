package week5

object tuples {
  def msort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (fst, snd) = xs splitAt n
      def merge(xs: List[T], ys: List[T]): List[T] =
        (xs, ys) match {
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case (x :: xs1, y :: ys1) => {
            if (lt(x, y)) x :: merge(xs1, ys)
            else y :: merge(xs, ys1)
          }
        }
      merge(msort(fst)(lt), msort(snd)(lt))
    }
  }                                               //> msort: [T](xs: List[T])(lt: (T, T) => Boolean)List[T]

  msort(List(1, 5, 2, 4, 5, 10, -4))((x, y) => x < y)
                                                  //> res0: List[Int] = List(-4, 1, 2, 4, 5, 5, 10)
  msort(List("chicken", "eggs", "potato"))((x, y) => x.compareTo(y) < 0)
                                                  //> res1: List[String] = List(chicken, eggs, potato)
}