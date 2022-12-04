package computation

import computation.PricingElements.PricingElm

import math._


class AsianOptionPricing (
                           elm: PricingElm,
                           optionType: String,
                           simTimes: Int = 100_000,
                           varReduce: Boolean = true,
                           randomSeed: Int = 1234
                         ) extends PricingOptions(elm, optionType, simTimes, varReduce, randomSeed) {

  def pricing: Double = {
    val pathMean = meanByRow(UnderlyingAssetPrices)
    val simMean = simulatedPathMean(pathMean)
    exp(-1 * r * horizon) * simMean
  }

}
