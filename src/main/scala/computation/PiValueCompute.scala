package computation

import scala.util.Random.{nextDouble, setSeed}
import scala.math.pow

object PiValueCompute {
  setSeed(1234)
  def compPi(n: Int): Double = {
    var cnt: Int = 0
    for (i <- 1 to n) {
      val x = nextDouble()
      val y = nextDouble()
      if (pow(x, 2) + pow(y, 2) <= 1) cnt += 1
    }
    cnt * 4.0 / n
  }

  def piValueSimulation(n: Int = 30, simTimes: Int = 1000000, randomSeed: Int = 1234): Double = {
    setSeed(randomSeed)
    var avgPi: Double = 0
    for (i <- 1 to n) {
      val pi = compPi(simTimes)
      avgPi += pi
    }
    avgPi / n
  }
}
