package week2_5

import scala.io.Source
import scala.collection.immutable.TreeMap

object Currying {

  type retrieve = () => Traversable[_]
  type transform = Traversable[_] => Traversable[_]
  type save = (Traversable[_]) => Unit

  def process(getValues: retrieve)(transformValues: transform)(setValues: save) = {
    val values = getValues()
    val transformedValues = transformValues(values)
    setValues(transformedValues)
  }                                               //> process: (getValues: week2_5.Currying.retrieve)(transformValues: week2_5.Cur
                                                  //| rying.transform)(setValues: week2_5.Currying.save)Unit
  def loadFromFile(path: String)(): Traversable[Array[String]] = {
    val src = Source.fromFile(path)
    src.getLines().map(_.split("\t")).toTraversable
  }                                               //> loadFromFile: (path: String)()Traversable[Array[String]]

  def writeToConsole(stringify: Any => String)(results: Traversable[_]): Unit = {
    results.foreach({
      x => println(stringify(x))
    })
  }                                               //> writeToConsole: (stringify: Any => String)(results: Traversable[_])Unit
  
  def writeToFile(stringify: Any => String)(results: Traversable[_]): Unit = {
    results.foreach({
      x => println(stringify(x))
    })
  }                                               //> writeToFile: (stringify: Any => String)(results: Traversable[_])Unit
  
  /**
    * Count the number of exceptions for each developer and write the results to console and file
    */
  def countExceptionsByDeveloper() {
    def countExceptions(input: Traversable[_]): Traversable[(Int, String)] = {
      object OrderingByCount extends Ordering[Int] {
        def compare(a: Int, b: Int) = b compare a
      }

      val downcastInput = input.asInstanceOf[Traversable[Array[String]]]
      val groups: Map[String, Traversable[Array[String]]] = downcastInput.groupBy(x => x(5)) // 5 is the developer column
      TreeMap[Int, String](groups.map(x => (x._2.size, x._1)).toArray: _*)(OrderingByCount)
    }

    val getCountString: Any => String = {
      case (key: Int, value: String) => value + " has " + key + " exceptions"
    }

    def processExceptionsFile = process(loadFromFile("/tmp/exceptions.csv")) _
    def countExceptionsByDeveloper = processExceptionsFile(countExceptions)
    countExceptionsByDeveloper(writeToConsole(getCountString))
  }                                               //> countExceptionsByDeveloper: ()Unit

  countExceptionsByDeveloper()                    //> bill.lee (Bill Lee) has 3028 exceptions
                                                  //| twauchope (Tanner Wauchope) has 1649 exceptions
                                                  //| virginia.wolfe (Virginia Wolfe) has 1149 exceptions
                                                  //| cescobar (Caleb Escobar) has 895 exceptions
                                                  //| cfarleigh (Clinton Farleigh) has 660 exceptions
                                                  //| ahuang (Angela Huang) has 538 exceptions
                                                  //| wlee (Wai-Ping Lee) has 467 exceptions
                                                  //| mjaynes (Mark Jaynes) has 296 exceptions
                                                  //| gberetta (Greg Beretta) has 162 exceptions
                                                  //| swong (Solomon Wong) has 151 exceptions
                                                  //| delee (Derek Lee) has 149 exceptions
                                                  //| cnishida (Chris Nishida) has 146 exceptions
                                                  //| jyen (John Yen) has 136 exceptions
                                                  //|  has 127 exceptions
                                                  //| salva (Salvador Maiorano) has 114 exceptions
                                                  //| natp (Nat Peterson) has 112 exceptions
                                                  //| don (Don Campbell) has 111 exceptions
                                                  //| kjarvis (Kevin Jarvis) has 107 exceptions
                                                  //| nng-quinn (Nora Ng-Quinn) has 105 exceptions
                                                  //| vvegunta (Vinay Vegunta) has 101 exceptions
                                                  //| dkasad (Darius Kasad) has 97 exceptions
                                                  //| bcreech (Barbara Creech) 
                                                  //| Output exceeds cutoff limit.
}