package week2_5

import scala.io.Source
import scala.collection.immutable.TreeMap

object Currying {

  type retrieve = Traversable[_]
  type transform = Traversable[_] => Traversable[_]
  type save = retrieve => Unit

  /**
   * The generic process function
   */
  def process(getValues: retrieve)(transformValues: transform)(setValues: save) = {
    val values = getValues
    val transformedValues = transformValues(values)
    setValues(transformedValues)
  }

  /**
   * Get values from file
   */
  def getValuesFromFile(path: String): Traversable[Array[String]] = {
    val src = Source.fromFile(path)
    src.getLines().map(_.split("\t")).toTraversable
  }

  /**
   * Write values to console
   */
  def writeValuesToConsole(stringify: Any => String)(results: Traversable[_]): Unit = {
    results.foreach({
      x => println(stringify(x))
    })
  }

  /**
   * Count the number of exceptions for each developer and write the results to console
   */
  def countExceptionsByDeveloper() {
    def countExceptions(input: Traversable[_]): Traversable[(Int, String)] = {
      object OrderingByCount extends Ordering[Int] {
        def compare(a: Int, b: Int) = b compare a
      }

      implicit def AnyToArrayString(x: Traversable[_]) = x.asInstanceOf[Traversable[Array[String]]]

      // BAD - Pattern matching is better, not downcasting is best
      val groups: Map[String, Traversable[Array[String]]] = input.groupBy(x => x(5)) // 5 is the developer column
      TreeMap[Int, String](groups.map(x => (x._2.size, x._1)).toArray: _*)(OrderingByCount)
    }

    val getCountString: Any => String = {
      case (key: Int, value: String) => value + " has " + key + " exceptions"
    }
    def valFromFile = getValuesFromFile("/tmp/exceptions.csv")
    def processExceptionsFromFile = process(valFromFile) _
    def processExceptionsToConsole = processExceptionsFromFile(_: transform)(writeValuesToConsole(getCountString))
    processExceptionsToConsole(countExceptions)
  }

  countExceptionsByDeveloper()
}