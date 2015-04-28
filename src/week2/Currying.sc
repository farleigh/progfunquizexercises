package Week2

object Currying {
  def product(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)              //> product: (f: Int => Int)(a: Int, b: Int)Int
  product(x => x)(1, 4)                           //> res0: Int = 24

  def factorial(n: Int): Int = product(x => x)(1, n)
                                                  //> factorial: (n: Int)Int
  factorial(5)                                    //> res1: Int = 120
  
 def mapReduce(f: Int => Int, unit: Int, combine: (Int, Int) => Int)(a: Int, b: Int): Int =
  if(a > b) unit
  else combine(f(a), mapReduce(f, unit, combine)(a + 1, b))
                                                  //> mapReduce: (f: Int => Int, unit: Int, combine: (Int, Int) => Int)(a: Int, b:
                                                  //|  Int)Int
  def product2(a: Int, b: Int) = mapReduce(x => x, 1, (x, y) => x * y)(a, b)
                                                  //> product2: (a: Int, b: Int)Int
  product2(1, 5)                                  //> res2: Int = 120
}