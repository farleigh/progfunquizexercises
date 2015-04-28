package progfun.week1

object TailRecursion {
  def factorial(x: Int): Int = {
  	def innerFactorial(x: Int, acc: Int): Int = if (x == 0) acc else innerFactorial(x - 1, x * acc)
  	innerFactorial(x, 1)
  }                                               //> factorial: (x: Int)Int
  
  factorial(2)                                    //> res0: Int = 2
  factorial(5)                                    //> res1: Int = 120
}