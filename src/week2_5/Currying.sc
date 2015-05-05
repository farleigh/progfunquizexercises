package week2_5

import scala.io.Source
import scala.collection.immutable.TreeMap

object Currying {

  type retrieve = () => Traversable[_]
  type transform = Traversable[_] => Traversable[_]
  type save = (Traversable[_]) => Unit

  /**
   * The generic process function
   */
  def process(getValues: retrieve)(transformValues: transform)(setValues: save) = {
    val values = getValues()
    val transformedValues = transformValues(values)
    setValues(transformedValues)
  }                                               //> process: (getValues: week2_5.Currying.retrieve)(transformValues: week2_5.Cur
                                                  //| rying.transform)(setValues: week2_5.Currying.save)Unit

  /**
   * Get values from file
   */
  def getValuesFromFile(path: String)(): Traversable[Array[String]] = {
    val src = Source.fromFile(path)
    src.getLines().map(_.split("\t")).toTraversable
  }                                               //> getValuesFromFile: (path: String)()Traversable[Array[String]]

  /**
   * Write values to console
   */
  def writeValuesToConsole(stringify: Any => String)(results: Traversable[_]): Unit = {
    results.foreach({
      x => println(stringify(x))
    })
  }                                               //> writeValuesToConsole: (stringify: Any => String)(results: Traversable[_])Uni
                                                  //| t

  /**
   * Count the number of exceptions for each developer and write the results to console
   */
  def countExceptionsByDeveloper() {
    def countExceptions(input: Traversable[_]): Traversable[(Int, String)] = {
      object OrderingByCount extends Ordering[Int] {
        def compare(a: Int, b: Int) = b compare a
      }

      // BAD - Pattern matching is better, not downcasting is best
      val downcastInput = input.asInstanceOf[Traversable[Array[String]]]
      val groups: Map[String, Traversable[Array[String]]] = downcastInput.groupBy(x => x(5)) // 5 is the developer column
      TreeMap[Int, String](groups.map(x => (x._2.size, x._1)).toArray: _*)(OrderingByCount)
    }

    val getCountString: Any => String = {
      case (key: Int, value: String) => value + " has " + key + " exceptions"
    }

    def processExceptionsFromFile = process(getValuesFromFile("/tmp/exceptions.csv")) _
    def processExceptionsToConsole = processExceptionsFromFile(_: transform)(writeValuesToConsole(getCountString))
    processExceptionsToConsole(countExceptions)
  }                                               //> countExceptionsByDeveloper: ()Unit

  countExceptionsByDeveloper()                    //> java.io.FileNotFoundException: /tmp/exceptions.csv (No such file or directo
                                                  //| ry)
                                                  //| 	at java.io.FileInputStream.open0(Native Method)
                                                  //| 	at java.io.FileInputStream.open(FileInputStream.java:195)
                                                  //| 	at java.io.FileInputStream.<init>(FileInputStream.java:138)
                                                  //| 	at scala.io.Source$.fromFile(Source.scala:91)
                                                  //| 	at scala.io.Source$.fromFile(Source.scala:76)
                                                  //| 	at scala.io.Source$.fromFile(Source.scala:54)
                                                  //| 	at week2_5.Currying$$anonfun$main$1.week2_5$Currying$$anonfun$$getValues
                                                  //| FromFile$1(week2_5.Currying.scala:25)
                                                  //| 	at week2_5.Currying$$anonfun$main$1$$anonfun$week2_5$Currying$$anonfun$$
                                                  //| processExceptionsFromFile$1$1.apply(week2_5.Currying.scala:57)
                                                  //| 	at week2_5.Currying$$anonfun$main$1$$anonfun$week2_5$Currying$$anonfun$$
                                                  //| processExceptionsFromFile$1$1.apply(week2_5.Currying.scala:57)
                                                  //| 	at week2_5.Currying$$anonfun$main$1.week2_5$Currying$$anonfun$$process$1
                                                  //| (week2_5.Currying.scala:16)
                                                  //| 	at week2_5.Currying$$anonfun$main$1$
                                                  //| Output exceeds cutoff limit.
}