//For some reason this does not work for lists of length=1

import scala.collection.immutable.ListMap
import scala.io.StdIn.readLine
import scala.math.min
import scala.language.postfixOps

object fractionalKnapsack {

  def getOptimalValue(capacity: Int, values: Array[Int], weights: Array[Int]): Double = {
    var W: Int = capacity
    var V: Double = 0
    var fracMap: Map[Double,Int] = Map()

    for(i <- 0 to (values.length - 1)) {
      fracMap += ((values(i)/weights(i)).asInstanceOf[Double] -> weights(i))
    }
    val fracKeys: List[Double] = fracMap.keySet.toList.sortWith(_ > _)

    if(fracKeys.length == 1) return (min(fracMap(fracKeys(0)),capacity) * fracKeys(0))
    else {
      for(j <- 0 to (fracKeys.length - 1)) {
        var a: Int = 0
        if(W == 0)
          return V
        a = min(fracMap(fracKeys(j)),W)
        V += a * fracKeys(j)
        W -= a
        }
    }
    return V
  }


  def main(args: Array[String]): Unit = {
    val s: String = readLine
    val sArray: Array[String] = s.split(" ")
    val n: Int = sArray(0).toInt
    val capacity: Int = sArray(1).toInt
    var stArray = new Array[String](n)

    var values  = new Array[Int](n)
    var weights = new Array[Int](n)

    var ok: Boolean = true
    while(ok) {
      for(i <- 0 to (n - 1)) {
        val ln = readLine
        if (ln == null || ln == "") ok = false
        if (ok) {
          stArray    = ln.split(" ")
          values(i)  = stArray(0).toInt
          weights(i) = stArray(1).toInt
        }
      }
    }
    val answer: Double = getOptimalValue(capacity, values, weights)
    println(f"$answer%.4f") // formatted to show 4 places after the decimal
  }

}
