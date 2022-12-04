package computation

import breeze.linalg.DenseVector
import computation.PricingElements.PricingElm

import math._

class LookBackOption (
                       elm: PricingElm,
                       optionType: String,
                       simTimes: Int = 100_000,
                       varReduce: Boolean = true,
                       randomSeed: Int = 1234
                     ) extends PricingOptions(elm, optionType, simTimes, varReduce, randomSeed) {

  def pricing: Double = {
    var pathVal = DenseVector.zeros[Double](simTimes)
    if (optionType == "call") {
      pathVal = maxByRow(UnderlyingAssetPrices)
    } else {
      pathVal = minByRow(UnderlyingAssetPrices)
    }
    val simMean = simulatedPathMean(pathVal)
    exp(-1 * r * horizon) * simMean
  }

}
